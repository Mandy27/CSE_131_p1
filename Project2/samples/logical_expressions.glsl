/* logical expressions 
   Note: Identifiers may be FieldAccess or VarExpr. Either is acceptable.
*/

void foo() {
  bool b;
  bool b2;

  b && true;
  b && b2;

  b || true;
  b || b2;
}