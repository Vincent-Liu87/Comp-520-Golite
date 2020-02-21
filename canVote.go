//Check if some one can vote
package valid

type person struct{
	age, vote int
	name string
}

func canVote(person p){
	if p.age>=18{
		println("You can vote!");
	}else{
		println("Sorry!");
	}
	return;
}

func main{
	person sam.age = 21;
	person sam.name = "Sam";
	person sam.vote = 1;
	canVote(sam);
}
