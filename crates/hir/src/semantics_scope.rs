use std::ops;

use hir_def::lower::LowerCtx;
use hir_def::path::Path;
use hir_def::resolver::{self, Resolver};
use hir_expand::HirFileId;
use rustc_hash::FxHashSet;
use syntax::ast;

use crate::db::HirDatabase;
use crate::source_analyzer::resolve_hir_path;
use crate::{Crate, Label, Local, Module, Name, PathResolution, ScopeDef, TraitId, TypeAlias};

/// `SemanticsScope` encapsulates the notion of a scope (the set of visible
/// names) at a particular program point.
///
/// It is a bit tricky, as scopes do not really exist inside the compiler.
/// Rather, the compiler directly computes for each reference the definition it
/// refers to. It might transiently compute the explicit scope map while doing
/// so, but, generally, this is not something left after the analysis.
///
/// However, we do very much need explicit scopes for IDE purposes --
/// completion, at its core, lists the contents of the current scope. The notion
/// of scope is also useful to answer questions like "what would be the meaning
/// of this piece of code if we inserted it into this position?".
///
/// So `SemanticsScope` is constructed from a specific program point (a syntax
/// node or just a raw offset) and provides access to the set of visible names
/// on a somewhat best-effort basis.
///
/// Note that if you are wondering "what does this specific existing name mean?",
/// you'd better use the `resolve_` family of methods.
#[derive(Debug)]
pub struct SemanticsScope<'a> {
    pub db: &'a dyn HirDatabase,
    pub file_id: HirFileId,
    pub resolver: Resolver,
}

impl SemanticsScope<'_> {
    pub fn module(&self) -> Module {
        Module { id: self.resolver.module() }
    }

    pub fn krate(&self) -> Crate {
        Crate { id: self.resolver.krate() }
    }

    pub(crate) fn resolver(&self) -> &Resolver {
        &self.resolver
    }

    /// Note: `VisibleTraits` should be treated as an opaque type, passed into `Type
    pub fn visible_traits(&self) -> VisibleTraits {
        let resolver = &self.resolver;
        VisibleTraits(resolver.traits_in_scope(self.db.upcast()))
    }

    /// Calls the passed closure `f` on all names in scope.
    pub fn process_all_names(&self, f: &mut dyn FnMut(Name, ScopeDef)) {
        let scope = self.resolver.names_in_scope(self.db.upcast());
        for (name, entries) in scope {
            for entry in entries {
                let def = match entry {
                    resolver::ScopeDef::ModuleDef(it) => ScopeDef::ModuleDef(it.into()),
                    resolver::ScopeDef::Unknown => ScopeDef::Unknown,
                    resolver::ScopeDef::ImplSelfType(it) => ScopeDef::ImplSelfType(it.into()),
                    resolver::ScopeDef::AdtSelfType(it) => ScopeDef::AdtSelfType(it.into()),
                    resolver::ScopeDef::GenericParam(id) => ScopeDef::GenericParam(id.into()),
                    resolver::ScopeDef::Local(binding_id) => match self.resolver.body_owner() {
                        Some(parent) => ScopeDef::Local(Local { parent, binding_id }),
                        None => continue,
                    },
                    resolver::ScopeDef::Label(label_id) => match self.resolver.body_owner() {
                        Some(parent) => ScopeDef::Label(Label { parent, label_id }),
                        None => continue,
                    },
                };
                f(name.clone(), def)
            }
        }
    }

    /// Resolve a path as-if it was written at the given scope. This is
    /// necessary a heuristic, as it doesn't take hygiene into account.
    pub fn speculative_resolve(&self, path: &ast::Path) -> Option<PathResolution> {
        let ctx = LowerCtx::new(self.db.upcast(), self.file_id);
        let path = Path::from_src(&ctx, path.clone())?;
        resolve_hir_path(self.db, &self.resolver, &path)
    }

    /// Iterates over associated types that may be specified after the given path (using
    /// `Ty::Assoc` syntax).
    pub fn assoc_type_shorthand_candidates<R>(
        &self,
        resolution: &PathResolution,
        mut cb: impl FnMut(&Name, TypeAlias) -> Option<R>,
    ) -> Option<R> {
        let def = self.resolver.generic_def()?;
        hir_ty::associated_type_shorthand_candidates(
            self.db,
            def,
            resolution.in_type_ns()?,
            |name, id| cb(name, id.into()),
        )
    }

    pub fn extern_crates(&self) -> impl Iterator<Item = (Name, Module)> + '_ {
        self.resolver.extern_crates_in_scope().map(|(name, id)| (name, Module { id }))
    }

    pub fn extern_crate_decls(&self) -> impl Iterator<Item = Name> + '_ {
        self.resolver.extern_crate_decls_in_scope(self.db.upcast())
    }

    pub fn has_same_self_type(&self, other: &SemanticsScope<'_>) -> bool {
        self.resolver.impl_def() == other.resolver.impl_def()
    }
}

#[derive(Debug)]
pub struct VisibleTraits(pub FxHashSet<TraitId>);

impl ops::Deref for VisibleTraits {
    type Target = FxHashSet<TraitId>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
