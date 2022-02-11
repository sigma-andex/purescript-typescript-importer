import * as ts from "typescript";
import * as fs from 'fs';

const filename = "./source.ts";
const program = ts.createProgram([filename], {});
const sourceFile = program.getSourceFile(filename);

// exports.createProgram = (fileNames) => () => ts.createProgram(fileNames, { allowJs: true })
// exports.createSourceFile = (fileName) => (sourceText) => () => ts.createSourceFile(fileName, sourceText, ts.ScriptTarget.ESNext, false, ts.ScriptKind.JS)

const typeChecker = program.getTypeChecker();

function recursivelyPrintVariableDeclarations(
    node: ts.Node, sourceFile: ts.SourceFile
) {
    if (node.kind === ts.SyntaxKind.FunctionDeclaration)  {
        const nodeText = node.getText(sourceFile);
        if(ts.isFunctionDeclaration(node)){
            node.parameters.forEach(node => {
                const type = typeChecker.getTypeAtLocation(node);
                const typeName = typeChecker.typeToString(type, node);
        
                //console.log(nodeText);
                console.log(`(${typeName})`);
                console.log(type);

            })
        }
    }

    node.forEachChild(child =>
        recursivelyPrintVariableDeclarations(child, sourceFile)
    );
}

sourceFile.forEachChild(child =>
  recursivelyPrintVariableDeclarations(child, sourceFile)
);
