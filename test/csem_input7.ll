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
  store i32 0, ptr %i1, align 4
  br label %L0

L0:                                               ; preds = %L13, %0
  %1 = load i32, ptr %i1, align 4
  %2 = icmp slt i32 %1, 6
  br i1 %2, label %L3, label %L15

tmpbp_L1:                                         ; No predecessors!
  unreachable

tmpbp_L2:                                         ; No predecessors!
  unreachable

L3:                                               ; preds = %L0
  store i32 5, ptr %j2, align 4
  br label %L4

L4:                                               ; preds = %L10, %L3
  %3 = load i32, ptr %i1, align 4
  %4 = getelementptr double, ptr @m, i32 %3
  %5 = load i32, ptr %i1, align 4
  %6 = getelementptr double, ptr @m, i32 %5
  %7 = load double, ptr %6, align 8
  %8 = load i32, ptr %i1, align 4
  %9 = load i32, ptr %j2, align 4
  %10 = mul i32 %8, %9
  %11 = sitofp i32 %10 to double
  %12 = fadd double %7, %11
  store double %12, ptr %4, align 8
  %13 = load i32, ptr %i1, align 4
  %14 = load i32, ptr %j2, align 4
  %15 = mul i32 %13, %14
  %16 = srem i32 %15, 3
  %17 = icmp eq i32 %16, 0
  br i1 %17, label %L7, label %L9

tmpbp_L5:                                         ; No predecessors!
  unreachable

tmpbp_L6:                                         ; No predecessors!
  unreachable

L7:                                               ; preds = %L4
  br label %L13

tmpbp_L8:                                         ; No predecessors!
  unreachable

L9:                                               ; preds = %L4
  %18 = load i32, ptr %j2, align 4
  %19 = sub i32 %18, 1
  store i32 %19, ptr %j2, align 4
  br label %L10

L10:                                              ; preds = %L13, %L9
  %20 = load i32, ptr %j2, align 4
  %21 = icmp sgt i32 %20, 0
  br i1 %21, label %L4, label %L13

tmpbp_L11:                                        ; No predecessors!
  unreachable

tmpbp_L12:                                        ; No predecessors!
  unreachable

L13:                                              ; preds = %L7, %L10
  br label %L10
  %22 = load i32, ptr %i1, align 4
  %23 = add i32 %22, 1
  store i32 %23, ptr %i1, align 4
  br label %L0

tmpbp_L14:                                        ; No predecessors!
  unreachable

L15:                                              ; preds = %L0
  %24 = load double, ptr @m, align 8
  %25 = load double, ptr getelementptr (double, ptr @m, i32 1), align 8
  %26 = load double, ptr getelementptr (double, ptr @m, i32 2), align 8
  %27 = load double, ptr getelementptr (double, ptr @m, i32 3), align 8
  %28 = load double, ptr getelementptr (double, ptr @m, i32 4), align 8
  %29 = load double, ptr getelementptr (double, ptr @m, i32 5), align 8
  %30 = call i32 (ptr, ...) @print(ptr @0, double %24, double %25, double %26, double %27, double %28, double %29)
  ret i32 0
}
