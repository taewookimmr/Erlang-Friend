.SUFFIXES:	.erl	.beam 

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS =  hello

all: compile
	${ERL} -pa 'C:/Programming/erlang/erlang_armstrong/6_compile_execute/makefileautomation' -s hello start \
	-s init stop
	
compile: ${MODS:%=%.beam} 

clean:
	rm -rf *.beam erl_crash.dump