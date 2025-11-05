; ModuleID = '<stdin>'
source_filename = "<stdin>"

@0 = private unnamed_addr constant [19 x i8] c"%d %3.2f %d %3.2f\0A\00", align 1

declare i32 @print(ptr, ...)

define i32 @main(i32 %a, i32 %c) {
  %a1 = alloca i32, align 4
  store i32 %a, ptr %a1, align 4
  %c2 = alloca i32, align 4
  store i32 %c, ptr %c2, align 4
  %b = alloca double, align 8
  %d = alloca double, align 8
  store i32 3, ptr %a1, align 4
  store double 4.000000e+00, ptr %b, align 8
  store i32 5, ptr %c2, align 4
  store double 6.000000e+00, ptr %d, align 8
  %1 = load i32, ptr %a1, align 4
  %2 = load double, ptr %b, align 8
  %3 = load i32, ptr %c2, align 4
  %4 = load double, ptr %d, align 8
  %5 = call i32 (ptr, ...) @print(ptr @0, i32 %1, double %2, i32 %3, double %4)
  ret i32 0
}
