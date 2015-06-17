CFSHARPC=fsharpc
CFLAGS=--nologo --debug:full

CMCS=mcs
CSFLAGS=-r:YP.dll


TARGET= fsedit.exe

SOURCES= ${CONTEXT_FACTS} \
	 ${CONTEXT_TYPES} \
	 resource.fs \
	 actions.fs \
	 ui.fs \
	 main.fs

CONTEXT=editor-context.dll
CONTEXTCS=editor-context.cs
CONTEXT_SRC=editor-context.P
CONTEXT_INIT_SRC=editor-initctx.fs
CONTEXT_INIT=editor-initctx.dll
CONTEXT_FACTS=editor-facts.fs
CONTEXT_TYPES=editor-types.fs


.PHONY=all clean

all: ${TARGET}

${TARGET}: ${SOURCES} ${CONTEXT} ${CONTEXT_INIT}
	${CFSHARPC} ${CFLAGS} ${SOURCES}  --out:${TARGET} -r coda.dll -r Unquote.dll -r ${CONTEXT}

${CONTEXTCS}: ${CONTEXT_SRC}
	echo "namespace Editor{\nusing System;\nusing System.Collections.Generic;\nusing YieldProlog;\npublic class EditorContext{\n " > ${CONTEXTCS}
	./ypc.exe < $< >> ${CONTEXTCS}
	echo "}\n}\n" >> ${CONTEXTCS}

${CONTEXT}: ${CONTEXTCS}
	${CMCS} -out:$@ -target:library ${CSFLAGS} $<

${CONTEXT_INIT}: ${CONTEXT_INIT_SRC}
	${CFSHARPC} ${CFLAGS} ${CONTEXT_FACTS} ${CONTEXT_TYPES} ${CONTEXT_INIT_SRC} -a --out:$@ -r coda.dll -r Unquote.dll -r ${CONTEXT}

clean:
	-rm -f *~ ${TARGET} ${CONTEXT} ${CONTEXTCS} ${CONTEXT_INIT} *.mdb



