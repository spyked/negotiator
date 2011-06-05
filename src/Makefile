all: BargainServer

BargainServer: BargainServer.hs WebBargain/*.hs
	ghc -o BargainServer $< --make -O2

clean:
	rm -f *.hi *.o */*.hi */*.o BargainServer
