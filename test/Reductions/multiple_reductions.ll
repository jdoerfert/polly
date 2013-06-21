; RUN: opt %loadPolly -O3 -polly -stats -polly-vectorizer=polly -polly-optimizer=isl -polly-no-tiling -polly-reduction-detection=basic -polly-code-generator=cloog -disable-output < %s 2>&1 | FileCheck %s
; ModuleID = 'multiple_reductions.s'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

;  #include <stdio.h>
;
;  #define N (1024 * 1024)
;
;  unsigned long A[N], B[N];
;  unsigned long sum1, sum2, mul1, mul2;
;
;  void multiple_reductions() {
;    int i;
;    for (i = 0; i < N; i++) {
;      sum1 += A[i];
;      sum2 += B[i];
;      mul1 *= A[i];
;      mul2 *= B[i];
;    }
;  }
;
;  int main() {
;    int i;
;    for (i = 0; i < N; i++) {
;      A[i] = ((i * 1337) % 1000);
;      B[i] = ((i * 42 * 77) % 500);
;      A[i] = (A[i] % 2 == 0 ? A[i] + 1 : A[i]);
;      B[i] = (B[i] % 2 == 0 ? B[i] + 1 : B[i]);
;    }
;
;    sum1 = sum2 = 0;
;    mul1 = mul2 = 1;
;
;    multiple_reductions();
;
;    printf("Sum1: %lu\n", sum1);
;    printf("Sum2: %lu\n", sum2);
;    printf("Mul1: %lu\n", mul1);
;    printf("Mul2: %lu\n", mul2);
;    return 0;
;  }

@A = common global [1048576 x i64] zeroinitializer, align 16
@sum1 = common global i64 0, align 8
@B = common global [1048576 x i64] zeroinitializer, align 16
@sum2 = common global i64 0, align 8
@mul1 = common global i64 0, align 8
@mul2 = common global i64 0, align 8
@.str = private unnamed_addr constant [11 x i8] c"Sum1: %lu\0A\00", align 1
@.str1 = private unnamed_addr constant [11 x i8] c"Sum2: %lu\0A\00", align 1
@.str2 = private unnamed_addr constant [11 x i8] c"Mul1: %lu\0A\00", align 1
@.str3 = private unnamed_addr constant [11 x i8] c"Mul2: %lu\0A\00", align 1

define void @multiple_reductions() nounwind uwtable {
bb:
  br label %bb1

bb1:                                              ; preds = %bb18, %bb
  %indvars.iv = phi i64 [ %indvars.iv.next, %bb18 ], [ 0, %bb ]
  %lftr.wideiv = trunc i64 %indvars.iv to i32
  %exitcond = icmp ne i32 %lftr.wideiv, 1048576
  br i1 %exitcond, label %bb2, label %bb19

bb2:                                              ; preds = %bb1
  %tmp = getelementptr inbounds [1048576 x i64]* @A, i64 0, i64 %indvars.iv
  %tmp3 = load i64* %tmp, align 8
  %tmp4 = load i64* @sum1, align 8
  %tmp5 = add i64 %tmp4, %tmp3
  store i64 %tmp5, i64* @sum1, align 8
  %tmp6 = getelementptr inbounds [1048576 x i64]* @B, i64 0, i64 %indvars.iv
  %tmp7 = load i64* %tmp6, align 8
  %tmp8 = load i64* @sum2, align 8
  %tmp9 = add i64 %tmp8, %tmp7
  store i64 %tmp9, i64* @sum2, align 8
  %tmp10 = getelementptr inbounds [1048576 x i64]* @A, i64 0, i64 %indvars.iv
  %tmp11 = load i64* %tmp10, align 8
  %tmp12 = load i64* @mul1, align 8
  %tmp13 = mul i64 %tmp12, %tmp11
  store i64 %tmp13, i64* @mul1, align 8
  %tmp14 = getelementptr inbounds [1048576 x i64]* @B, i64 0, i64 %indvars.iv
  %tmp15 = load i64* %tmp14, align 8
  %tmp16 = load i64* @mul2, align 8
  %tmp17 = mul i64 %tmp16, %tmp15
  store i64 %tmp17, i64* @mul2, align 8
  br label %bb18

bb18:                                             ; preds = %bb2
  %indvars.iv.next = add i64 %indvars.iv, 1
  br label %bb1

bb19:                                             ; preds = %bb1
  ret void
}

define i32 @main() nounwind uwtable {
bb:
  br label %bb1

bb1:                                              ; preds = %bb40, %bb
  %indvars.iv = phi i64 [ %indvars.iv.next, %bb40 ], [ 0, %bb ]
  %lftr.wideiv = trunc i64 %indvars.iv to i32
  %exitcond = icmp ne i32 %lftr.wideiv, 1048576
  br i1 %exitcond, label %bb2, label %bb41

bb2:                                              ; preds = %bb1
  %tmp = mul nsw i64 %indvars.iv, 1337
  %tmp3 = trunc i64 %tmp to i32
  %tmp4 = srem i32 %tmp3, 1000
  %tmp5 = sext i32 %tmp4 to i64
  %tmp6 = getelementptr inbounds [1048576 x i64]* @A, i64 0, i64 %indvars.iv
  store i64 %tmp5, i64* %tmp6, align 8
  %tmp7 = trunc i64 %indvars.iv to i32
  %tmp8 = mul i32 %tmp7, 3234
  %tmp9 = srem i32 %tmp8, 500
  %tmp10 = sext i32 %tmp9 to i64
  %tmp11 = getelementptr inbounds [1048576 x i64]* @B, i64 0, i64 %indvars.iv
  store i64 %tmp10, i64* %tmp11, align 8
  %tmp12 = getelementptr inbounds [1048576 x i64]* @A, i64 0, i64 %indvars.iv
  %tmp13 = load i64* %tmp12, align 8
  %tmp14 = and i64 %tmp13, 1
  %tmp15 = icmp eq i64 %tmp14, 0
  br i1 %tmp15, label %bb16, label %bb20

bb16:                                             ; preds = %bb2
  %tmp17 = getelementptr inbounds [1048576 x i64]* @A, i64 0, i64 %indvars.iv
  %tmp18 = load i64* %tmp17, align 8
  %tmp19 = add i64 %tmp18, 1
  br label %bb23

bb20:                                             ; preds = %bb2
  %tmp21 = getelementptr inbounds [1048576 x i64]* @A, i64 0, i64 %indvars.iv
  %tmp22 = load i64* %tmp21, align 8
  br label %bb23

bb23:                                             ; preds = %bb20, %bb16
  %tmp24 = phi i64 [ %tmp19, %bb16 ], [ %tmp22, %bb20 ]
  %tmp25 = getelementptr inbounds [1048576 x i64]* @A, i64 0, i64 %indvars.iv
  store i64 %tmp24, i64* %tmp25, align 8
  %tmp26 = getelementptr inbounds [1048576 x i64]* @B, i64 0, i64 %indvars.iv
  %tmp27 = load i64* %tmp26, align 8
  %tmp28 = and i64 %tmp27, 1
  %tmp29 = icmp eq i64 %tmp28, 0
  br i1 %tmp29, label %bb30, label %bb34

bb30:                                             ; preds = %bb23
  %tmp31 = getelementptr inbounds [1048576 x i64]* @B, i64 0, i64 %indvars.iv
  %tmp32 = load i64* %tmp31, align 8
  %tmp33 = add i64 %tmp32, 1
  br label %bb37

bb34:                                             ; preds = %bb23
  %tmp35 = getelementptr inbounds [1048576 x i64]* @B, i64 0, i64 %indvars.iv
  %tmp36 = load i64* %tmp35, align 8
  br label %bb37

bb37:                                             ; preds = %bb34, %bb30
  %tmp38 = phi i64 [ %tmp33, %bb30 ], [ %tmp36, %bb34 ]
  %tmp39 = getelementptr inbounds [1048576 x i64]* @B, i64 0, i64 %indvars.iv
  store i64 %tmp38, i64* %tmp39, align 8
  br label %bb40

bb40:                                             ; preds = %bb37
  %indvars.iv.next = add i64 %indvars.iv, 1
  br label %bb1

bb41:                                             ; preds = %bb1
  store i64 0, i64* @sum2, align 8
  store i64 0, i64* @sum1, align 8
  store i64 1, i64* @mul2, align 8
  store i64 1, i64* @mul1, align 8
  call void @multiple_reductions()
  %tmp42 = load i64* @sum1, align 8
  %tmp43 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([11 x i8]* @.str, i64 0, i64 0), i64 %tmp42) nounwind
  %tmp44 = load i64* @sum2, align 8
  %tmp45 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([11 x i8]* @.str1, i64 0, i64 0), i64 %tmp44) nounwind
  %tmp46 = load i64* @mul1, align 8
  %tmp47 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([11 x i8]* @.str2, i64 0, i64 0), i64 %tmp46) nounwind
  %tmp48 = load i64* @mul2, align 8
  %tmp49 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([11 x i8]* @.str3, i64 0, i64 0), i64 %tmp48) nounwind
  ret i32 0
}

declare i32 @printf(i8*, ...)

; Check for 4 valid reductions all vectorizable
; CHECK: 4 {{.*}} Number of valid reduction accesses
; CHECK: 4 {{.*}} Number of vectorized reduction accesses
