import * as ts from "typescript";
declare module "typescript" {
	export function getDeclarationModifierFlagsFromSymbol(
		s: ts.Symbol
	): ts.ModifierFlags;
	export function signatureHasRestParameter(signature: ts.Signature): boolean;
	interface Symbol {
		type?: ts.Type;
		parent: Symbol | undefined
	}
	interface Declaration {
		expression?: ts.Expression;
	}
	interface Signature {
		thisParameter?: Symbol;
	}
	interface Type {
		intrinsicName?: string;
	}
}
