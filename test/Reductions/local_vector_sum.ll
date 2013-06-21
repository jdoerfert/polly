; RUN: opt %loadPolly %defaultOpts -polly-reductions -polly-opt-isl -polly-codegen -polly-vectorizer=polly -analyze -stats < %s 2>&1 | FileCheck %s
; ModuleID = 'global_vector_sum.ll'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

;  int vec_sum(int * restrict A) {
;    int i, sum = 0;
;    for (i = 0; i < 100; i++)
;      sum += A[i];
;    return sum;
;  }

; Function Attrs: nounwind uwtable
define i32 @vec_sum(i32* noalias %A) #0 {
bb:
  %sum = alloca i32, align 4
  store i32 0, i32* %sum, align 4
  br label %bb.split

bb.split:                                         ; preds = %bb
  br label %bb1

bb1:                                              ; preds = %bb8, %bb.split
  %indvars.iv = phi i64 [ %indvars.iv.next, %bb8 ], [ 0, %bb.split ]
  %tmp = trunc i64 %indvars.iv to i32
  %tmp2 = icmp slt i32 %tmp, 100
  br i1 %tmp2, label %bb3, label %bb9

bb3:                                              ; preds = %bb1
  %tmp4 = getelementptr inbounds i32* %A, i64 %indvars.iv
  %tmp5 = load i32* %tmp4, align 4
  %tmp6 = load i32* %sum, align 4
  %tmp7 = add nsw i32 %tmp6, %tmp5
  store i32 %tmp7, i32* %sum, align 4
  br label %bb8

bb8:                                              ; preds = %bb3
  %indvars.iv.next = add i64 %indvars.iv, 1
  br label %bb1

bb9:                                              ; preds = %bb1
  %tmp8 = load i32* %sum, align 4
  ret i32 %tmp8
}

attributes #0 = { nounwind uwtable }

; CHECK: 1 {{.*}} Number of valid reduction accesses
; CHECK: 1 {{.*}} Number of vectorized reduction accesses
