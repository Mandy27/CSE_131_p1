/* while 
   Note: Identifiers may be FieldAccess or VarExpr. Either is acceptable.
*/

void foo() {
  bool b;
  int i;

  b = true;
  i = 0;

  while (b)
    b = false;

  while (i < 3) {
    i++;
  }

}