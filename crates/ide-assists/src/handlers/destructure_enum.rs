use crate::{
    assist_context::{AssistContext, Assists, SourceChangeBuilder},
    utils::ref_field_expr::determine_ref_and_parens,
};
use hir::{Adt, Semantics};
use syntax::{
    ast::{self, make, AstNode, Expr, FieldExpr, HasName, IdentPat, LetStmt},
    ted,
};

use ide_db::{
    assists::{AssistId, AssistKind},
    defs::Definition,
    search::{FileReference, SearchScope},
    syntax_helpers::suggest_name,
    text_edit::TextRange,
    RootDatabase, SnippetCap,
};

// Assist: destructure_enum
//
// Destructures an enum value to a match expression.
//
// ```
// # //- minicore: option
// fn main() {
//     let t = Some(1);
//     $0t;
// }
// ```
// ->
// ```
// fn main() {
//     let t = Some(1);
//     match t {
//         Some(_) => {}
//         None => {}
//     };
// }
// ```
pub(crate) fn destructure_enum(acc: &mut Assists, ctx: &AssistContext<'_>) -> Option<()> {
    let expr = ctx.find_node_at_offset::<Expr>()?;
    let enum_ = resolve_enum_def(&ctx.sema, &expr)?;

    for v in enum_.variants(ctx.sema.db) {
        dbg!(v);
    }

    dbg!(enum_);

    acc.add(
        AssistId("destructure_enum", AssistKind::RefactorRewrite),
        "Destructure enum with match",
        expr.syntax().text_range(),
        |builder| {
            builder.insert(expr.syntax().text_range().start(), "match ");
            builder.insert(expr.syntax().text_range().end(), " {");
            builder.insert(expr.syntax().text_range().end(), " }");
        },
    )
}

fn resolve_enum_def(sema: &Semantics<'_, RootDatabase>, expr: &ast::Expr) -> Option<hir::Enum> {
    sema.type_of_expr(expr)?.adjusted().autoderef(sema.db).find_map(|ty| match ty.as_adt() {
        Some(Adt::Enum(e)) => Some(e),
        _ => None,
    })
}
