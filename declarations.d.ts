import * as ts from "typescript";
declare module "typescript" {
	export function getDeclarationModifierFlagsFromSymbol(
		s: ts.Symbol
	): ts.ModifierFlags;
	interface Symbol {
		type?: ts.Type;
	}
	interface Declaration {
		expression?: ts.Expression;
	}
	interface Signature {
		thisParameter?: Symbol;
	}
}
