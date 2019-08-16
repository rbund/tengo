package compiler

import (
  "strings"
  "path/filepath"
  "io/ioutil"
  
	"github.com/d5/tengo/compiler/ast"
	"github.com/d5/tengo/compiler/parser"
	"github.com/d5/tengo/objects"
)

func (c *Compiler) checkCyclicImports(node ast.Node, modulePath string) error {
	if c.modulePath == modulePath {
		return c.errorf(node, "cyclic module import: %s", modulePath)
	} else if c.parent != nil {
		return c.parent.checkCyclicImports(node, modulePath)
	}

	return nil
}

// rbund added, extracted from Compiler.Compile() (compiler.go)
func (c *Compiler) importModule(node *ast.ImportExpr) (*objects.CompiledFunction, error) {
  if c.allowFileImport {
    moduleName := node.ModuleName
    if !strings.HasSuffix(moduleName, ".tengo") {
      moduleName += ".tengo"
    }

    modulePath, err := filepath.Abs(moduleName)
    if err != nil {
      return nil, c.errorf(node, "module file path error: %s", err.Error())
    }
    if err := c.checkCyclicImports(node, modulePath); err != nil {
      return nil, err
    }
    // loading source:
    moduleSrc, err := ioutil.ReadFile(moduleName)
    if err != nil {
      return nil, c.errorf(node, "module file read error: %s", err.Error())
    }
    return c.compileModule(node, moduleName, modulePath, moduleSrc)
  }
  return nil, c.errorf(node, "module '%s' not found", node.ModuleName)
}

func (c *Compiler) compileModule(node ast.Node, moduleName, modulePath string, src []byte) (*objects.CompiledFunction, error) {
	if err := c.checkCyclicImports(node, modulePath); err != nil {
		return nil, err
	}

	compiledModule, exists := c.loadCompiledModule(modulePath)
	if exists {
		return compiledModule, nil
	}

	modFile := c.file.Set().AddFile(moduleName, -1, len(src))
	p := parser.NewParser(modFile, src, nil)
	file, err := p.ParseFile()
	if err != nil {
		return nil, err
	}

	symbolTable := NewSymbolTable()

	// inherit builtin functions
	for _, sym := range c.symbolTable.BuiltinSymbols() {
		symbolTable.DefineBuiltin(sym.Index, sym.Name)
	}

	// no global scope for the module
	symbolTable = symbolTable.Fork(false)

	// compile module
	moduleCompiler := c.fork(modFile, modulePath, symbolTable)
	if err := moduleCompiler.Compile(file); err != nil {
		return nil, err
	}

	// code optimization
	moduleCompiler.optimizeFunc(node)

	compiledFunc := moduleCompiler.Bytecode().MainFunction
	compiledFunc.NumLocals = symbolTable.MaxSymbols()

	c.storeCompiledModule(modulePath, compiledFunc)

	return compiledFunc, nil
}

func (c *Compiler) loadCompiledModule(modulePath string) (mod *objects.CompiledFunction, ok bool) {
	if c.parent != nil {
		return c.parent.loadCompiledModule(modulePath)
	}

	mod, ok = c.compiledModules[modulePath]

	return
}

func (c *Compiler) storeCompiledModule(modulePath string, module *objects.CompiledFunction) {
	if c.parent != nil {
		c.parent.storeCompiledModule(modulePath, module)
	}

	c.compiledModules[modulePath] = module
}
