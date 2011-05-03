#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

/*Program to output the current time in Swatch Internet Time beats*/

int parseargs(int, char**, char*);

int main(int argc, char **argv){
	float beats;
	beats=(time(0)+3600)%86400/86.4;
	if(parseargs(argc,argv,"-h")){
		printf("beats - outputs current Swatch Internet Time\n");
		printf("Options:\n");
		printf("-i   omit decimal places\n");
		printf("-a   omit leading @-sign\n");
		printf("-n   omit newline\n");
		printf("-h   print this help screen\n");
		printf("\n\n");
		exit(EXIT_SUCCESS);
	}
	if(!parseargs(argc,argv,"-a")) putchar('@');
	if(parseargs(argc,argv,"-i")){
		printf("%03i",(int)beats);
	}else{
		printf("%03.2f",beats);
	}
	if(!parseargs(argc,argv,"-n")) putchar('\n');
	return EXIT_SUCCESS;
}

int parseargs(int argc, char **argv, char *arg){
	int i=0;
	if(argc<2) return 0;
	for (i=1;i<argc;i++){
		if(!strcmp(argv[i],arg)) return 1;
	}
	return 0;
}
