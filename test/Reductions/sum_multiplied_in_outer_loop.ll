; RUN: opt %loadPolly -O3 -polly -stats -polly-vectorizer=polly -polly-optimizer=isl -polly-no-tiling -polly-reduction-detection=basic -polly-code-generator=cloog -disable-output < %s 2>&1 | FileCheck %s
; ModuleID = 'sum_multiplied_in_outer_loop.s'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

;  #include <stdio.h>
;
;  #define M (1024 * 8)
;
;  unsigned long sum;
;  int Matrix[M][M];
;
;  void sum_multiplied_in_outer_loop(int N) {
;    int i, j, k;
;    sum = 0;
;    for (i = 0; i < N; i++) {
;      for (j = 0; j < M; j++) {
;        for (k = 0; k < M; k++) {
;          sum += Matrix[j][k];
;        }
;      }
;      sum *= 2;
;    }
;  }
;
;  int main(int argc, char **argv) {
;    int j, k;
;    for (j = 0; j < M; j++)
;      for (k = 0; k < M; k++)
;        Matrix[j][k] = (j * k * argc) % 137;
;
;    sum_multiplied_in_loop(argc);
;    printf("Sum with N=%i: %lu\n", argc, sum);
;    return 0;
;  }

@sum = common global i64 0, align 8
@Matrix = common global [8192 x [8192 x i32]] zeroinitializer, align 16
@.str = private unnamed_addr constant [20 x i8] c"Sum with N=%i: %lu\0A\00", align 1

define void @sum_multiplied_in_outer_loop(i32 %N) nounwind uwtable {
bb:
  store i64 0, i64* @sum, align 8
  br label %bb5

bb5:                                              ; preds = %bb22, %bb
  %i.0 = phi i32 [ 0, %bb ], [ %tmp23, %bb22 ]
  %tmp = icmp slt i32 %i.0, %N
  br i1 %tmp, label %bb6, label %bb24

bb6:                                              ; preds = %bb5
  br label %bb7

bb7:                                              ; preds = %bb18, %bb6
  %indvars.iv1 = phi i64 [ %indvars.iv.next2, %bb18 ], [ 0, %bb6 ]
  %lftr.wideiv3 = trunc i64 %indvars.iv1 to i32
  %exitcond4 = icmp ne i32 %lftr.wideiv3, 8192
  br i1 %exitcond4, label %bb8, label %bb19

bb8:                                              ; preds = %bb7
  br label %bb9

bb9:                                              ; preds = %bb16, %bb8
  %indvars.iv = phi i64 [ %indvars.iv.next, %bb16 ], [ 0, %bb8 ]
  %lftr.wideiv = trunc i64 %indvars.iv to i32
  %exitcond = icmp ne i32 %lftr.wideiv, 8192
  br i1 %exitcond, label %bb10, label %bb17

bb10:                                             ; preds = %bb9
  %tmp11 = getelementptr inbounds [8192 x [8192 x i32]]* @Matrix, i64 0, i64 %indvars.iv1, i64 %indvars.iv
  %tmp12 = load i32* %tmp11, align 4
  %tmp13 = sext i32 %tmp12 to i64
  %tmp14 = load i64* @sum, align 8
  %tmp15 = add i64 %tmp14, %tmp13
  store i64 %tmp15, i64* @sum, align 8
  br label %bb16

bb16:                                             ; preds = %bb10
  %indvars.iv.next = add i64 %indvars.iv, 1
  br label %bb9

bb17:                                             ; preds = %bb9
  br label %bb18

bb18:                                             ; preds = %bb17
  %indvars.iv.next2 = add i64 %indvars.iv1, 1
  br label %bb7

bb19:                                             ; preds = %bb7
  %tmp20 = load i64* @sum, align 8
  %tmp21 = shl i64 %tmp20, 1
  store i64 %tmp21, i64* @sum, align 8
  br label %bb22

bb22:                                             ; preds = %bb19
  %tmp23 = add nsw i32 %i.0, 1
  br label %bb5

bb24:                                             ; preds = %bb5
  ret void
}

define i32 @main(i32 %argc, i8** %argv) nounwind uwtable {
bb:
  br label %bb5

bb5:                                              ; preds = %bb15, %bb
  %indvars.iv1 = phi i64 [ %indvars.iv.next2, %bb15 ], [ 0, %bb ]
  %lftr.wideiv3 = trunc i64 %indvars.iv1 to i32
  %exitcond4 = icmp ne i32 %lftr.wideiv3, 8192
  br i1 %exitcond4, label %bb6, label %bb16

bb6:                                              ; preds = %bb5
  br label %bb7

bb7:                                              ; preds = %bb13, %bb6
  %indvars.iv = phi i64 [ %indvars.iv.next, %bb13 ], [ 0, %bb6 ]
  %lftr.wideiv = trunc i64 %indvars.iv to i32
  %exitcond = icmp ne i32 %lftr.wideiv, 8192
  br i1 %exitcond, label %bb8, label %bb14

bb8:                                              ; preds = %bb7
  %tmp = mul nsw i64 %indvars.iv1, %indvars.iv
  %tmp9 = trunc i64 %tmp to i32
  %tmp10 = mul nsw i32 %tmp9, %argc
  %tmp11 = srem i32 %tmp10, 137
  %tmp12 = getelementptr inbounds [8192 x [8192 x i32]]* @Matrix, i64 0, i64 %indvars.iv1, i64 %indvars.iv
  store i32 %tmp11, i32* %tmp12, align 4
  br label %bb13

bb13:                                             ; preds = %bb8
  %indvars.iv.next = add i64 %indvars.iv, 1
  br label %bb7

bb14:                                             ; preds = %bb7
  br label %bb15

bb15:                                             ; preds = %bb14
  %indvars.iv.next2 = add i64 %indvars.iv1, 1
  br label %bb5

bb16:                                             ; preds = %bb5
  call void @sum_multiplied_in_outer_loop(i32 %argc)
  %tmp17 = load i64* @sum, align 8
  %tmp18 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([20 x i8]* @.str, i64 0, i64 0), i32 %argc, i64 %tmp17) nounwind
  ret i32 0
}

declare i32 @printf(i8*, ...)

; Check if there is at least one loop invalidating a reduction access
; (the outer one should do that at least once!) and if we find exactly one
; valid reduction access (sum += M[i][j] with the inner two loops).

; CHECK: Number of loops invalidating reduction access
; CHECK: 1 {{.*}} Number of valid reduction accesses
; CHECK: 1 {{.*}} Number of vectorized reduction accesses
