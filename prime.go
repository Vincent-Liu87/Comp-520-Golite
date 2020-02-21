//check number is prime
package valid

type num int
func checkPrime(num n) int{
	for i:=0;i<n;i++{
		if n%i = 0{
			return 0;
		}
	}
	return 1;
}

func main(){
	num n = 29;
	if x:=checkPrime(29);x=0{
		println("it's a prime.");
	}else if x=1{
		println("it's not a prime.")
	}
}
