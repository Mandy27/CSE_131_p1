/* for 
   Note: Identifiers may be FieldAccess or VarExpr. Either is acceptable.
*/

void foo() {
  bool b;
  int i;

  b = true;
  i = 0;

  for (i = 1; b != true; i++)
    b = false;

  for (i = 1; b != true; i++) {
    b = false;
    i++;
  }
}