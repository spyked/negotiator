all: site/BargainServer

site/BargainServer: BargainServer.hs
	ghc -o site/BargainServer $< --make -O2

clean:
	rm -f *.hi *.o */*.hi */*.o site/BargainServer
