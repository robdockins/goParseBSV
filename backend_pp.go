// Copyright (c) 2016-2017 Rishiyur Nikhil and Bluespec, Inc.  All Rights Reserved.

// This is back-end processor of ASTs created by goParseBSV, a parser
// for BSV files.

// Specifically, this is a pretty printer that traverses an AST
// and prints it out with reasonable indentation etc.

// More generally, it provides an example of how to structure other
// back-ends that traverse the AST and do some processing on them.
// The key idea is that we define a local interface ast_pp with a
// method pp. Then we define the pp method for each AST type AstFoo.
// To process recursive components of interface type AST

package	goParseBSV

import (
	// golang packages
	"fmt"
	"os"
)

// ================================================================
// Top-level function exported out of this file

func AST_pp (fout *os.File, indent string, ast AST) () {
	conv (ast).pp (fout, indent)
}

// ================================================================
// Each kind of AST has its own struct definition
// They are all 'union'd via the AST interface

// AST is the generic type of Abstract Syntax Trees
// PP_AST2 is the method to pretty print an AST
type ast_pp_ifc interface {
	pp (fout *os.File, indent string)
}

// Convert an AST to an ast_pp_ifc
func conv (ast AST) ast_pp_ifc {
	ast_pp := ast.(ast_pp_ifc)
	return ast_pp
}

// ================================================================
// Pretty printer for lists
func ppList (asts [] AST, fout *os.File, indent string) {
	if asts == nil {
		fmt.Fprintf (fout, "\n" + indent + "[]")
	} else {
		fmt.Fprintf (fout, "\n" + indent + "[ ")
		indent2 := indent + "  "
		for j, ast := range(asts) {
			if (j != 0) { fmt.Fprintf (fout, "\n" + indent + ", ") }
			conv(ast).pp (fout, indent2)
		}
		fmt.Fprintf (fout, "\n" + indent + "]")
	}
}

func ppIdeList (ides [] *AstIde, fout *os.File, indent string) {
	if ides == nil {
		fmt.Fprintf (fout, "\n" + indent + "[]")
	} else {
		fmt.Fprintf (fout, "\n" + indent + "[ ")
		indent2 := indent + "  "
		for j, ide := range(ides) {
			if (j != 0) { fmt.Fprintf (fout, "\n" + indent + ", ") }
			ide.pp (fout, indent2)
		}
		fmt.Fprintf (fout, "\n" + indent + "]")
	}
}

func ppKindList (kinds [] uint, fout *os.File, indent string) {
	if kinds == nil {
		fmt.Fprintf (fout, "\n" + indent + "[]")
	} else {
		fmt.Fprintf (fout, "\n" + indent + "[ ")
		for j, kind := range(kinds) {
			if (j != 0) { fmt.Fprintf (fout, "\n" + indent + ", ") }
			if kind == KindValue {
				fmt.Fprintf (fout, "KindValue")
			} else {
				fmt.Fprintf (fout, "KindNumeric")
			}
		}
		fmt.Fprintf (fout, "\n" + indent + "]")
	}
}

// ================================================================
// Items that occur in many parts of the grammar

// Ides
func (ast *AstIde) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstIde \"%s\")", ast.LeafValue.StringVal)
}

// Attribute instances
func (ast *AstAttrInstance) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstAttrInstance")
	indent2 := indent + "  "
	ppIdeList (ast.Ides, fout, indent2)
	if ast.Vals == nil {
		fmt.Fprintf (fout, "\n" + indent2 + "[]")
	} else {
		fmt.Fprintf (fout, "\n" + indent2 + "[ ")
		indent3 := indent2 + "    "
		for j, val := range(ast.Vals) {
			if (j != 0) { fmt.Fprintf (fout, "\n" + indent2 + ", ") }
			if val != nil {
				fmt.Fprintf (fout, "(Just ")
				conv(val).pp (fout, indent3)
				fmt.Fprintf (fout, ")")
			} else {
				fmt.Fprintf (fout, "Nothing")
			}
		}
		fmt.Fprintf (fout, "\n" + indent2 + "]")
	}
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// ================================================================
// ASTs for types

// AstTypeNum is a BSV numeric type, e.g.,  "16"
func (ast *AstTypeNum) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstTypeNum %d)", ast.LeafValue.IntVal)
}

// AstTypeVar is a BSV type variable (identifier with lowercase first letter)
// E.g.,  "t"
func (ast *AstTypeVar) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstTypeVar \"%s\")", ast.LeafValue.StringVal)
}

// AstTypeConstructed is a BSV "TypeConstructor #(typeExpr, ..., typeExpr)"
func (x AstTypeConstructed) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstTypeConstructed \"%s\"", x.Constructor.LeafValue.StringVal)
	indent2 := indent + "  "
	ppList (x.Args, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstTypedefDefinedAsStruct is a BSV "struct { type field; ... ; type field }"
func (ast *AstTypedefDefinedAsStruct) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstTypedefDefinedAsStruct")
	indent2 := indent + "  "
	ppIdeList (ast.StructMemberNames, fout, indent2)
        ppList (ast.StructMemberTypes, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstTypedefDefinedAsTaggedUnion is a BSV "union tagged { type field; ... ; type field }"
func (ast *AstTypedefDefinedAsTaggedUnion) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstTypedefDefinedAsTaggedUnion \n")
	indent2 := indent + "  "
	for j, memberName := range (ast.TaggedUnionMemberNames) {
		if (j > 0) { fmt.Fprintf (fout, indent2) }
		fieldType := ast.TaggedUnionMemberTypes [j]
		conv (fieldType).pp (fout, indent)
		fmt.Fprintf (fout, " ")
		memberName.pp (fout, indent)
		fmt.Fprintf (fout, ";\n")
	}
	fmt.Fprintf (fout, indent)
	fmt.Fprintf (fout, ")")
}

// AstTypedefDefinedAsEnum is a BSV "enum { label [=val], ... , label [=val] }"
func (ast *AstTypedefDefinedAsEnum) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstTypedefDefinedAsEnum")
	indent2 := indent + "  "
	ppIdeList (ast.TypedefEnumElements, fout, indent2)
	if ast.TypedefEnumVals == nil {
		fmt.Fprintf (fout, "\n" + indent2 + "[]")
	} else {
		fmt.Fprintf (fout, "\n" + indent2 + "[ ")
		indent3 := indent2 + "    "
		for j, val := range(ast.TypedefEnumVals) {
			if (j != 0) { fmt.Fprintf (fout, "\n" + indent2 + ", ") }
			if val != nil {
				fmt.Fprintf (fout, "(Just ")
				val.pp (fout, indent3)
				fmt.Fprintf (fout, ")")
			} else {
				fmt.Fprintf (fout, "Nothing")
			}
		}
		fmt.Fprintf (fout, "\n" + indent2 + "]")
	}
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstTypedefDefinee is the new type being defined in a typedef: Type #(typeFormal,...,typeFormal)
// where each typeformal is "type typevar" or "numeric type typevar"
func (ast *AstTypedefDefinee) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstTypedefDefinee")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.TypeConstructor).pp (fout, indent2)
	ppIdeList (ast.TypeFormals, fout, indent2)
	ppKindList (ast.TypeFormalKinds, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// ================================================================
// ASTs for exprs

// AstNum is a BSV numeric constant
// E.g., 23
func (ast *AstNum) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstNum %d)", ast.LeafValue.IntVal)
}

// AstString is a BSV string
// E.g.,  "x"
func (ast *AstString) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstString \"%s\")", ast.LeafValue.StringVal)
}

// AstExpr is the parse of a BSV expression applying Expr0 to Exprs
func (ast *AstExpr) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstExpr ")
	indent2 := indent + "  "
	conv(ast.Expr0).pp (fout, indent2)
	ppList (ast.Exprs, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstCondPredicate is the parse of a condition, in if, while, rule conditions, method conditions, etc.
// i.e., conjunct &&& conjunct &&& ...
func (ast *AstCondPredicate) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstCondPredicate")
	indent2 := indent + "  "
	ppList (ast.Conjuncts, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstCondPattern is the parse of: expr matches pattern
func (ast *AstCondPattern) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstCondPattern")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Expr).pp (fout, indent2)
	conv (ast.Pattern).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstBlock is the parse of:
//     action ... endaction
//     actionvalue ... endactionvalue
//     begin ... end
func (ast *AstBlock) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstBlock")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	switch ast.BlockKind {
	case "action":
		fmt.Fprintf (fout, "BlockKindAction")
	case "actionvalue":
		fmt.Fprintf (fout, "BlockKindActionValue")
	case "begin":
		fmt.Fprintf (fout, "BlockKindBegin")
	default:
		fmt.Fprintf (fout, "(undefined)")
	}
	fmt.Fprintf (fout, "\n" + indent2)
	if ast.BlockName != nil {
		fmt.Fprintf (fout, "(Just ")
		ast.BlockName.pp (fout, indent2)
		fmt.Fprintf (fout, ")")
	} else {
		fmt.Fprintf (fout, "Nothing")
	}
	ppList (ast.Stmts, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstReturn is the parse of: return Expr
func (ast *AstReturn) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstReturn")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Expr).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstStructExpr is the parse of: Ide {member:Expr, ... }
func (ast *AstStructExpr) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstStructExpr")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Name).pp (fout, indent2)
	ppIdeList (ast.MemberNames, fout, indent2)
	ppList (ast.MemberExprs, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstTaggedUnionExpr is the parse of: tagged Ide Expr
func (ast *AstTaggedUnionExpr) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstTaggedUnionExpr")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Name).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	if ast.Expr != nil {
		fmt.Fprintf (fout, "(Just ")
		conv (ast.Expr).pp (fout, indent2)
		fmt.Fprintf (fout, ")")
	} else {
		fmt.Fprintf (fout, "Nothing")
	}
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstCase is the parse of: case ... endcase
func (ast *AstCase) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstCase")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Expr).pp (fout, indent2)
	ppList (ast.Labels, fout, indent2)
	ppList (ast.Exprs, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstPatternCase is the parse of: case () matches ... endcase
func (ast *AstPatternCase) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstPatternCase")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Expr).pp (fout, indent2)
	ppList (ast.Patterns, fout, indent2)
	ppList (ast.Exprs, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// ===============================================================
// ASTs for statements

// VarDecl is the parse of 'type x1 = e1, x2 = e2, ...;"
func (ast *AstVarDecl) pp (fout *os.File, indent string) () {
	indent2 := indent + "  "
	fmt.Fprintf (fout, "(AstVarDecl")
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Type).pp (fout, indent2)
	if ast.VarInits == nil {
		fmt.Fprintf (fout, "\n" + indent2 + "[]")
	} else {
		fmt.Fprintf (fout, "\n" + indent2 + "[ ")
		indent3 := indent + "    "
		for j, varinit := range(ast.VarInits) {
			if (j != 0) { fmt.Fprintf (fout, "\n" + indent2 + ", ") }
			conv(varinit).pp (fout, indent3)
		}
		fmt.Fprintf (fout, "\n" + indent2 + "]")
	}
	fmt.Fprintf (fout, "\n" + indent + ")")
}

func ppKind (kind uint, fout *os.File, indent string) {
	switch kind {
	case BindingKindNone:
		fmt.Fprintf (fout, "BindingKindNone")
	case BindingKindEq:
		fmt.Fprintf (fout, "BindingKindEq")
	case BindingKindLArrow:
		fmt.Fprintf (fout, "BindingKindLArrow")
	default:
		fmt.Fprintf (fout, "(undefined)")
	}
}

// An Init in a VarDecl, 'xJ = eJ' or 'Ide [e]...[e] = Expr;'
func (ast *AstVarInit) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstVarInit")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Ide).pp (fout, indent2)
	ppList (ast.ArrayDims, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	ppKind (ast.Kind, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Init).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstLet is the parse of a BSV statement: "let x = e" or "let x <- e"
func (ast *AstLet) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstLet")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Ide).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	ppKind (ast.Kind, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Expr).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstMatch is the parse of a BSV statement: "match pattern = e"  or "match pattern <- e"
func (ast *AstMatch) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstMatch")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Pattern).pp (fout, indent)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Expr).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstAssign is the parse of a BSV statement: "lhs = e" or "ide <- e"
func (ast *AstAssign) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstAssign")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Lhs).pp (fout, indent2)
	ppKind (ast.Kind, fout, indent2)
	conv (ast.Rhs).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstRule is the parse of "rule ... endrule"
func (ast *AstRule) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstRule")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Name).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	if ast.Cond != nil {
		fmt.Fprintf (fout, "(Just ")
		conv (ast.Cond).pp (fout, indent2)
		fmt.Fprintf (fout, ")")
	} else {
		fmt.Fprintf (fout, "Nothing")
	}
	ppList (ast.Stmts, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstFunctionProto is the parse of "function type name (type formal, ..., type formal)"
func (ast *AstFunctionProto) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstFunctionProto")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.ResultType).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Name).pp (fout, indent)
	ppList (ast.Formals, fout, indent2)
	ppList (ast.FormalTypes, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstFunctionDef is the parse of "function ... endfunction" or "function .... = e"
func (ast *AstFunctionDef) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstFunctionDef")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	ast.Proto.pp (fout, indent2)
	ppList (ast.Provisos, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Body).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstMethodDef is the parse of "method ... endmethod" or "method .... = e"
func (ast *AstMethodDef) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstMethodDef")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.ResultType).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Name).pp (fout, indent2)
	ppList (ast.Formals, fout, indent2)
	ppList (ast.FormalTypes, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	if ast.Cond != nil {
		fmt.Fprintf (fout, "(Just ")
		conv (ast.Cond).pp (fout, indent2)
		fmt.Fprintf (fout, ")")
	} else {
		fmt.Fprintf (fout, "Nothing")
	}
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Body).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstModuleDef is the parse of "module ... endmodule"
func (ast *AstModuleDef) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstModuleDef")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	if ast.ModuleType != nil {
		fmt.Fprintf (fout, "(Just ")
		conv (ast.ModuleType).pp (fout, indent2)
		fmt.Fprintf (fout, ")")
	} else {
		fmt.Fprintf (fout, "Nothing")
	}

	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Name).pp (fout, indent)
	ppList (ast.FormalParams, fout, indent2)
	ppList (ast.FormalParamTypes, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.IfcType).pp (fout, indent2)
	ppList (ast.Provisos, fout, indent2)
	ppList (ast.Stmts, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstInterfaceDef is the parse of "interface ... endinterface" within a module
func (ast *AstInterfaceDef) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstInterfaceDef")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	if ast.Type != nil {
		fmt.Fprintf (fout, "(Just ")
		conv (ast.Type).pp (fout, indent2)
		fmt.Fprintf (fout, ")")
	} else {
		fmt.Fprintf (fout, "Nothing")
	}
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Name).pp (fout, indent2)
	ppList (ast.MethodAndIfcDefs, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstInterfaceAssign is the parse of "interface ... = e" within a module
func (ast *AstInterfaceAssign) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstInterfaceAssign")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	if ast.Type != nil {
		fmt.Fprintf (fout, "(Just ")
		conv (ast.Type).pp (fout, indent2)
		fmt.Fprintf (fout, ")")
	} else {
		fmt.Fprintf (fout, "Nothing")
	}
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Name).pp (fout, indent)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Val).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstIf is the parse of: "if (E) S1 else S2"
func (ast *AstIf) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstIf")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.ExprCond).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.StmtThen).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	if ast.StmtElse != nil {
		fmt.Fprintf (fout, "(Just ")
		conv (ast.StmtElse).pp (fout, indent2)
		fmt.Fprintf (fout, ")")
	} else {
		fmt.Fprintf (fout, "Nothing")
	}
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstFor is the parse of: "for (type x = e; e; x = ...) stmt"
func (ast *AstFor) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstFor")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.LoopInit).pp (fout, indent)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.LoopCond).pp (fout, indent)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.LoopIncr).pp (fout, indent)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.LoopBody).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstWhile is the parse of: "while (e) stmt"
func (ast *AstWhile) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstWhile")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.LoopCond).pp (fout, indent)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.LoopBody).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstInterfaceExpr is the parse of expression: interface ... endinterface
func (ast *AstInterfaceExpr) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstInterfaceExpr")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	if ast.Type != nil {
		fmt.Fprintf (fout, "(Just ")
		conv (ast.Type).pp (fout, indent2)
		fmt.Fprintf (fout, ")")
	} else {
		fmt.Fprintf (fout, "Nothing")
	}
	ppList (ast.MethodAndIfcDefs, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// ================================================================
// StmtFSM

// AstFSMseq is the parse of: 'seq stmt stmt ... endseq'
func (ast *AstFSMseq) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstFSMseq")
	indent2 := indent + "  "
	ppList (ast.Stmts, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstFSMpar is the parse of: 'par stmt stmt ... endpar'
func (ast *AstFSMpar) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstFSMpar")
	indent2 := indent + "  "
	ppList (ast.Stmts, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstFSMif is the parse of: "if (E) S1 else S2"
func (ast *AstFSMif) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstFSMif")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.ExprCond).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.StmtThen).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	if ast.StmtElse != nil {
		fmt.Fprintf (fout, "(Just ")
		conv (ast.StmtElse).pp (fout, indent2)
		fmt.Fprintf (fout, ")")
	} else {
		fmt.Fprintf (fout, "Nothing")
	}
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstFSMfor is the parse of: 'for (init_stmts; cond; incr_stmts) fsmstmt'
func (ast *AstFSMfor) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstFSMfor")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.LoopInit).pp (fout, indent)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.LoopCond).pp (fout, indent)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.LoopIncr).pp (fout, indent)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.LoopBody).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstFSMwhile is the parse of: "while (e) stmt"
func (ast *AstFSMwhile) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstFSMwhile")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.LoopCond).pp (fout, indent)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.LoopBody).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstFSMrepeat is the parse of: "repeat (n) stmt"
func (ast *AstFSMrepeat) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstFSMrepeat")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.LoopCount).pp (fout, indent)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.LoopBody).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstFSMreturn is the parse of: 'return'
func (ast *AstFSMreturn) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "AstFSMreturn")
}

// AstFSMbreak is the parse of: 'break'
func (ast *AstFSMbreak) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "AstFSMbreak")
}

// AstFSMcontinue is the parse of: 'continue'
func (ast *AstFSMcontinue) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "AstFSMcontinue")
}

// ================================================================
// ASTs for patterns
// Syntax of patterns:
//     Pattern variables
//         . *                                        wildcard
//         . x                                        pattern varIde
//     Pattern constants
//         23
//         2.5
//         "Hello"
//         Foo                                        enum labels
//     Tagged unions
//         tagged tag pattern
//     Structs
//         tagged structname {member:pattern, ...}
//     Tuples
//         { pattern, ... }

// AstPatternVarIde is the parse of a pattern .* or .x
func (ast *AstPatternVarIde) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstPatternVarIde")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.varIde).pp (fout, indent)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstPatternConst is the parse of a pattern const integer, real, string, or Enum label
func (ast *AstPatternConst) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstPatternConst")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.constant).pp (fout, indent)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstStructPattern is the parse of:  tagged StructName { MemberName: Pattern, ..., }
// and Tuple patterns (StructName is "Tuple", MemberNames are tuple_1, tuple_2, ...
func (ast *AstStructPattern) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstStructPattern")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.StructName).pp (fout, indent2)
	ppList (ast.MemberNames, fout, indent2)
	ppList (ast.MemberPatterns, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstTaggedUnionPattern is the parse of:  tagged UnionName [ Pattern ]
func (ast *AstTaggedUnionPattern) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstTaggedUnionPattern")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.TaggedUnionName).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	if ast.MemberPattern != nil {
		fmt.Fprintf (fout, "(Just ")
		conv (ast.MemberPattern).pp (fout, indent2)
		fmt.Fprintf (fout, ")")
	} else {
		fmt.Fprintf (fout, "Nothing")
	}
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// ================================================================
// Top-level constructs in a package

// AstPackage is parse of 'package pkgStmt ... pkgStmt endpackage'
func (ast *AstPackage) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstPackage")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	if ast.PackageName != nil {
		fmt.Fprintf (fout, "(Just ")
		conv (ast.PackageName).pp (fout, indent2)
		fmt.Fprintf (fout, ")")
	} else {
		fmt.Fprintf (fout, indent2 + "Nothing")
	}

	ppList (ast.PackageStmts, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstImport is parse of 'import x :: *;'
func (ast *AstImport) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstImport")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.PackageName).pp (fout, indent)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstExport is parse of 'export x, y, z (..), w, ...;'
func (ast *AstExport) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstExport")
	indent2 := indent + "  "
	ppIdeList (ast.Ides, fout, indent2)
	if ast.WithMembers == nil {
		fmt.Fprintf (fout, "\n" + indent2 + "[]")
	} else {
		fmt.Fprintf (fout, "\n" + indent2 + "[ ")
		for j, b := range(ast.WithMembers) {
			if (j != 0) { fmt.Fprintf (fout, "\n" + indent2 + ", ") }
			if b { fmt.Fprintf (fout, "True")
			} else { fmt.Fprintf (fout, "False") }
		}
		fmt.Fprintf (fout, "\n" + indent2 + "]")
	}
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstImportBDPI is parse of 'import "BDPI" function_proto'
func (ast *AstImportBDPI) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstImportBDPI")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	// TODO: ImportBDPI does not yet handle renaming of the imported function
	ast.Proto.pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstTypedef is the parse of a BSV statement: "typedef typeDefinedAs newtype deriving (typeclass, ...);"
func (ast *AstTypedef) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "(AstTypedef")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.TypedefDefinee).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.TypedefDefinedAs).pp (fout, indent2)
	ppIdeList (ast.TypeclassIdes, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// AstIfcDecl is the parse of a top-level BSV declaration: 'interface ... endinterface'
func (ast *AstIfcDecl) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstIfcDecl")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.Ifc).pp (fout, indent2)
	ppList (ast.SubIfcOrMethodDecls, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// Sub-interface within an interface declaration
func (ast *AstIfcDeclSubIfcDecl) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstIfcDeclSubIfcDecl")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.SubIfcName).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.SubIfcType).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// Method declaration within an interface declaration
func (ast *AstIfcDeclMethodDecl) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstIfcDeclMethodDecl")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.MethodName).pp (fout, indent2)
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.ReturnType).pp (fout, indent2)
	ppIdeList (ast.ArgNames, fout, indent2)
	ppList (ast.ArgTypes, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

//     instance typeclassIde # ( type { , type } ) [ provisos ] ;
//         { varAssign ; | functionDef | moduleDef }
//     endinstance [ : typeclassIde ]
func (ast *AstInstance) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(AstInstance")
	indent2 := indent + "  "
	fmt.Fprintf (fout, "\n" + indent2)
	conv (ast.TypeclassIde).pp (fout, indent2)
	ppList (ast.Types, fout, indent2)
	ppList (ast.Provisos, fout, indent2)
	ppList (ast.Stmts, fout, indent2)
	fmt.Fprintf (fout, "\n" + indent + ")")
}

// ================================================================
