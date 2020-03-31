import * as ts from "typescript";
declare module "typescript" {
	export function getDeclarationModifierFlagsFromSymbol(
		s: ts.Symbol
	): ts.ModifierFlags;
	type OldSymbol = ts.Symbol;
	export interface Symbol extends OldSymbol {
		type?: ts.Type;
	}
	type OldDeclaration = ts.Declaration;
	export interface Declaration extends OldDeclaration {
		expression?: ts.Expression;
	}
}
