/* if statements 
   Note: Identifiers may be FieldAccess or VarExpr. Either is acceptable.
*/

void foo() {
  bool b;
  int i;

  if (b) {
    i = 2;
  }

  if (b) {
    i = 5;
  } else {
    i = 6;
  }
}