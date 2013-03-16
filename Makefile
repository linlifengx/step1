sources=main.cpp ast.cpp parser.cpp token.cpp
headers=ast.hpp parser.hpp

all:parser lex bin

clean:
	rm -rf parser.cpp token.cpp parser.hpp sprc

parser:parser.y
	bison -d -o parser.cpp $<

lex:token.l
	flex -o token.cpp $<

bin:$(sources) $(headers)
	g++ -o sprc $(sources) -Iinclude -I. `llvm-config --cxxflags --ldflags --libs core jit native all-targets asmparser`
