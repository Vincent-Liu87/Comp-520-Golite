//type error: apply selector on non struct type
package fault12
func main(){
	x int
	print(x.field1)
}
