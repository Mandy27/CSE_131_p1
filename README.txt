CSE 131 Project 2
By Mandy Ngo, Tyler Nguyen

Extra Credit:

1) Handled Errors: Switch Statement with No body 
                Switch Statement with Default, but no Case

2) Handle Declaration with Initializer:
  Add a new constructor, and a new variable in VarDecl class

input:
void main() {
    int i =10;
}

output:
   Program: 
  5   FnDecl: 
         (return type) Type: void
  5      Identifier: main
         (body) StmtBlock: 
  6         VarDecl: 
               Type: int 
  6            Identifier: i
            AssignExpr:
  6            IntConstant: 10

3) Vector constructor worked
4) Swizzle: only accept a combination of xywz
