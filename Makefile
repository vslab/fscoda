default: example.exe example-short.exe
	mono example-short.exe

ypc.exe: YP.dll ypc.fs
	fsharpc --out:$@ -r YP.dll ypc.fs

coda.dll: YP.dll Unquote.dll coda.fs
	fsharpc --out:$@ -a -r YP.dll -r Unquote.dll coda.fs

example-context.cs: ypc.exe example.P
	(echo "namespace Example {"; \
	 echo "using System;" ; \
	 echo "using System.Collections.Generic;" ; \
	 echo "using YieldProlog;" ; \
	 echo "public class ExampleContext {" ; \
	 mono ypc.exe < example.P ; \
	 echo "}}" ; \
	) > $@

example-context.dll: YP.dll example-context.cs
	gmcs -out:$@ -target:library -r:YP.dll example-context.cs

example-ctx.dll: YP.dll example-facts.fs example-types.fs example-sysutils.fs example-ctx.fs
	fsharpc -a --out:$@ -r coda.dll -r Unquote.dll -r example-context.dll example-facts.fs example-types.fs example-sysutils.fs example-ctx.fs

example.exe: coda.dll example-context.dll example-facts.fs example-types.fs example-sysutils.fs example.fs
	fsharpc --out:$@ -r coda.dll -r Unquote.dll -r example-context.dll example-facts.fs example-types.fs example-sysutils.fs example.fs

example-short.exe: coda.dll example-ctx.dll example-context.dll example-facts.fs example-types.fs example-sysutils.fs example-short.fs
	fsharpc --out:$@ -r coda.dll -r Unquote.dll -r example-ctx.dll -r example-context.dll example-short.fs
