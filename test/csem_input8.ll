; ModuleID = '<stdin>'
source_filename = "<stdin>"

@m = global [6 x double] zeroinitializer
@0 = private unnamed_addr constant [37 x i8] c"%5.1f %5.1f %5.1f %5.1f %5.1f %5.1f\0A\00", align 1

declare i32 @print(ptr, ...)

define i32 @main(i32 %i, i32 %j) {
  %i1 = alloca i32, align 4
  store i32 %i, ptr %i1, align 4
  %j2 = alloca i32, align 4
  store i32 %j, ptr %j2, align 4
  store i32 1, ptr %i1, align 4
  store i32 8, ptr %j2, align 4
  br label %L1

L1:                                               ; preds = %0
  %1 = load i32, ptr %i1, align 4
  %2 = getelementptr [6 x double], ptr @m, i32 0, i32 %1
  %3 = load i32, ptr %i1, align 4
  %4 = getelementptr [6 x double], ptr @m, i32 0, i32 %3
  %5 = load double, ptr %4, align 8
  %6 = load i32, ptr %i1, align 4
  %7 = load i32, ptr %j2, align 4
  %8 = mul i32 %6, %7
  %9 = sitofp i32 %8 to double
  %10 = fadd double %5, %9
  store double %10, ptr %2, align 8
  %11 = load i32, ptr %i1, align 4
  %12 = getelementptr [6 x double], ptr @m, i32 0, i32 %11
  %13 = load double, ptr %12, align 8
  %14 = fcmp ogt double %13, 3.000000e+01
  br i1 %14, label %L0, label %L2

L0:                                               ; preds = %L1
  br label %L04

L13:                                              ; No predecessors!
  br label %L2

L2:                                               ; preds = %L1, %L13
  %15 = load i32, ptr %j2, align 4
  %16 = sub i32 %15, 1
  store i32 %16, ptr %j2, align 4
  br label <badref>

L3:                                               ; No predecessors!
  br label %L04

L04:                                              ; preds = %L0, %L3
  %17 = load i32, ptr %i1, align 4
  %18 = add i32 %17, 1
  store i32 %18, ptr %i1, align 4
  %19 = load i32, ptr %i1, align 4
  %20 = icmp slt i32 %19, 4
  br i1 %20, label %L4, label %L6

L4:                                               ; preds = %L04
  store i32 8, ptr %j2, align 4
  br label <badref>

L5:                                               ; No predecessors!
  br label %L6

L6:                                               ; preds = %L04, %L5
  %21 = load double, ptr @m, align 8
  %22 = load double, ptr getelementptr ([6 x double], ptr @m, i32 0, i32 1), align 8
  %23 = load double, ptr getelementptr ([6 x double], ptr @m, i32 0, i32 2), align 8
  %24 = load double, ptr getelementptr ([6 x double], ptr @m, i32 0, i32 3), align 8
  %25 = load double, ptr getelementptr ([6 x double], ptr @m, i32 0, i32 4), align 8
  %26 = load double, ptr getelementptr ([6 x double], ptr @m, i32 0, i32 5), align 8
  %27 = call i32 (ptr, ...) @print(ptr @0, double %21, double %22, double %23, double %24, double %25, double %26)
  ret i32 0
}
