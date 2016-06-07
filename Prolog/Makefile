rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

JAVAS=$(call rwildcard,WHIM_MiniNarrativeParser/,*.java)

all: parser/dist/build/parser/parser WHIM_MiniNarrativeParser/mininarrative.jar

parser/dist/build/parser/parser: parser/MiniNarrative.hs parser/parser.cabal
	cd parser && cabal sandbox init
	cd parser && cabal update
	cd parser && cabal configure
	cd parser && cabal install --only dependencies
	cd parser && cabal build

.PHONY: all clean

clean:
	cd parser && cabal clean
	rm WHIM_MiniNarrativeParser/mininarrative.jar

WHIM_MiniNarrativeParser/mininarrative.jar: $(JAVAS)
	eclipse -noSplash -data WHIM_MiniNarrativeParser -application org.eclipse.jdt.apt.core.aptBuild
	cd WHIM_MiniNarrativeParser && ant
