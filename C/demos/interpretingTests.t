  $ ./demoInterpret.exe <<-EOF
  > int Fact(int value) {
  >   if (value <= 1) {
  >     return 1;
  >   }
  >   else {
  >     return value * Fact(value - 1);
  >   }
  > }
  > int main() {
  >   int n = 5;
  >   return Fact(n);
  > }
  > EOF
  120

  $ ./demoInterpret.exe <<-EOF
  > void Fact(int value) {
  >   
  > }
  > int main() {
  >   int n, i;
  >   int fact = 1;
  >   int n = 5;
  >   if (n < 0)
  >   {
  >      return -1;
  >   }
  >   else {
  >     for (i = 1; i <= n; i++) {
  >       fact *= i;
  >     }
  >   }
  >  return fact;
  > }
  > EOF
  120

  $ ./demoInterpret.exe <<-EOF
  > char main(int number, char** table) {
  >   char* coeff = "abiba";
  >   int16_t coeff2 = 2;
  >   coeff += coeff2;
  >   return coeff[0];
  > }
  > EOF
  i

  $ ./demoInterpret.exe <<-EOF
  > char main(int number, char** table) {
  >   char* coeff = "abiba";
  >   coeff[2] = 'a';
  >   int16_t coeff2 = 2;
  >   coeff += coeff2;
  >   return coeff[0];
  > }
  > EOF
  a

  $ ./demoInterpret.exe <<-EOF
  > char main(int number, char** table) {
  >   char* coeff = "abiba";
  >   coeff[5] = 'a';
  >   int16_t coeff2 = 2;
  >   coeff += coeff2;
  >   return coeff[0];
  > }
  > EOF
  Index of the coeff is out of range

  $ ./demoInterpret.exe <<-EOF
  > int main() {
  >   int* array = malloc(20);
  >   int sum = 0;
  >   int n = 5;
  >   for (int i = 0; i <= n - 1; i++) {
  >       array[i] = i + 1;
  >     }
  >   for (int i = 0; i <= n - 1; i++) {
  >       sum += array[i];
  >     }
  >   return sum;
  > }
  > EOF
  15

  $ ./demoInterpret.exe <<-EOF
  > int main() {
  >   char* array = malloc(20);
  >   int* new_arr = (int*)array;
  >   return new_arr[8];
  > }
  > EOF
  Index of the pointer is greater than pointer size

  $ ./demoInterpret.exe <<-EOF
  > char main() {
  >   char* array = "test_string";
  >   int* new_arr = (int*)array;
  >   char* prev_arr = (char*)array;
  >   return prev_arr[0];
  > }
  > EOF
  t

