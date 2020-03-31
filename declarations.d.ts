import * as ts from "typescript";
declare module "typescript" {
	export function getDeclarationModifierFlagsFromSymbol(
		s: ts.Symbol
	): ts.ModifierFlags;
	type OldSymbol = ts.Symbol;
	interface Symbol {
		type?: ts.Type;
	}
	interface Declaration {
		expression?: ts.Expression;
	}
}
