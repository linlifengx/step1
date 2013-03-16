long global_a=1,global_b=2;
double global_c=3.14;

/*
main function  is the entry of the program
*/
void main(){
	global_a = global_a + 1;
	global_c = global_c + global_b;
	printL(global_a);println(); //print 2
	printD(global_c);println(); //print 5.14

	long x = 112/3;
	printL(x);println(); //print 37

	double y;
	[x,y] = test1(); //x=100 y=3.333
	
	[,y] = test2(); //y=3.333

	printL(x);println();
	printD(y);println();
}

//return two values
long,double test1(){
	return 100,3.333;
}

//as test1, but another syntax
[long a,double b] test2(){
	a = 100,b=3.333;
	return;
}
