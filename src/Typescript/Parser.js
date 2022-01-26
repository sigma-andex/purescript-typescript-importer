"use strict"

const ts = require('typescript')

exports.createProgram = (fileNames) => () => ts.createProgram(fileNames, { allowJs: true })

exports.getTypeChecker = (program) => () => program.getTypeChecker()

exports.getTypeAtLocation = (typeChecker) => (node) => typeChecker.getTypeAtLocation(node)

exports.typeToString = (typeChecker) => (node) => (type) => typeChecker.typeToString(type, node)

exports.createSourceFile = (fileName) => (sourceText) => ts.createSourceFile(fileName, sourceText, ts.ScriptTarget.ESNext, false, ts.ScriptKind.JS)

exports.getSourceFile = (program) => (fileName) => () =>
    program.getSourceFile(fileName)

exports.getSourceFiles = (program) => () => program.getSourceFiles()

exports.getSourceFileName = (sourceFile) => sourceFile.getName()

exports.getSourceFileChildren = (sourceFile) => sourceFile.getChildren()

exports.getChildren = (node) => node.getChildren()

exports.isTypeAliasDeclarationImpl = (node) =>
    ts.isTypeAliasDeclaration(node) ? node : null

exports.isTypeLiteralNodeImpl = (node) =>
    ts.isTypeLiteralNode(node) ? node : null

exports.isPropertySignatureImpl = (node) =>
    ts.isPropertySignature(node) ? node : null

exports.isFunctionDeclarationImpl = (node) =>
    ts.isFunctionDeclaration(node) ? node : null

exports.isTypeReferenceNodeImpl = (node) =>
    ts.isTypeReferenceNode(node) ? node : null

exports.isVariableStatementImpl = (node) =>
    ts.isVariableStatement(node) ? node : null

exports.isModuleDeclarationImpl = (node) =>
    ts.isModuleDeclaration(node) ? node : null

exports.isModuleBlockImpl = (node) =>
    ts.isModuleBlock(node) ? node : null
