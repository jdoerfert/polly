; RUN: opt %loadPolly -polly-collect-reductions -polly-basic-ri -analyze -basicaa < %s | FileCheck %s
; ModuleID = 'multiple_loops_array_sum.ll'
;
; int f(int * __restrict__ A) {
;   int i, j, sum = 1;
;   for (i = 0; i < 100; i++) {
;     sum *= 2;
;     for (j = 0; j < 100; j++) {
;       sum += A[i+j];
;     }
;     sum *= 2;
;   }
;   return sum;
; }

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define i32 @f(i32* noalias %A) #0 {
entry:
  %sum.04.reg2mem = alloca i32
  %sum.12.reg2mem = alloca i32
  br label %entry.split

entry.split:                                      ; preds = %entry
  store i32 0, i32* %sum.04.reg2mem
  br label %for.body

for.body:                                         ; preds = %for.inc5, %entry.split
  %indvars.iv23 = phi i64 [ 0, %entry.split ], [ %3, %for.inc5 ]
  %sum.04.reload = load i32* %sum.04.reg2mem
  %mul = shl nsw i32 %sum.04.reload, 1
  store i32 %mul, i32* %sum.12.reg2mem
  br label %for.inc

for.inc:                                          ; preds = %for.inc, %for.body
  %indvars.iv1 = phi i64 [ 0, %for.body ], [ %1, %for.inc ]
  %sum.12.reload = load i32* %sum.12.reg2mem
  %0 = add i64 %indvars.iv23, %indvars.iv1
  %arrayidx = getelementptr i32* %A, i64 %0
  %tmp5 = load i32* %arrayidx, align 4
  %add4 = add nsw i32 %tmp5, %sum.12.reload
  %1 = add nuw nsw i64 %indvars.iv1, 1
  %exitcond1 = icmp eq i64 %1, 100
  store i32 %add4, i32* %sum.12.reg2mem
  br i1 %exitcond1, label %for.inc5, label %for.inc

for.inc5:                                         ; preds = %for.inc
  %2 = load i32* %sum.12.reg2mem
  %3 = add nuw nsw i64 %indvars.iv23, 1
  %exitcond2 = icmp eq i64 %3, 100
  store i32 %2, i32* %sum.04.reg2mem
  br i1 %exitcond2, label %for.end7, label %for.body

for.end7:                                         ; preds = %for.inc5
  %4 = load i32* %sum.04.reg2mem
  ret i32 %4
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

; Verify that there was one (the inner) reduction detected
; CHECK-NOT: RA: Reduction Loop: Loop at depth 1
; CHECK: RA: Reduction Loop: Loop at depth 2
; CHECK-NOT: RA: Reduction Loop:
