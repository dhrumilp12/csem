; ModuleID = '<stdin>'
source_filename = "<stdin>"

@0 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @print(ptr, ...)

define i32 @main() {
  %x = alloca i32, align 4
  %y = alloca i32, align 4
  %z = alloca i32, align 4
  store i32 0, ptr %x, align 4
  store i32 1, ptr %y, align 4
  store i32 0, ptr %z, align 4
  %1 = load i32, ptr %y, align 4
  %2 = icmp ne i32 %1, 0
  br i1 %2, label %L1, label %L0

L0:                                               ; preds = %0
  store i32 1, ptr %z, align 4
  br i1 true, label %L1, label %L2

L1:                                               ; preds = %L0, %0
  br label %L2

L2:                                               ; preds = %L0, %L1
  %3 = load i32, ptr %z, align 4
  %4 = call i32 (ptr, ...) @print(ptr @0, i32 %3)
  ret i32 0
}
