/* switch statements 
   Note: Identifiers may be FieldAccess or VarExpr. Either is acceptable.
*/

void foo() {
  int i;
  
  switch (i) {
    case 1:
      i = 2;
    case 2:
      i = 1;
    default:
      i = 1;
  }
}