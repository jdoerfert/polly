; RUN: opt %loadPolly -polly-collect-reductions -polly-basic-ri -analyze -basicaa < %s | FileCheck %s
;
; int f(int *__restrict__ S, int *__restrict__ B) {
;   int i, j, k, sum = 0;
;   for (i = 0; i < 100; i++) {
;     for (j = 0; j < 100; j++) {
;       for (k = 0; k < 100; k++) {
;         S[j] += B[i + j + k];
;         sum += B[i+j+k];
;       }
;     }
;   }
;   return sum;
; }
;
; ModuleID = 'multiple_reduction_loops_multiple_reductions.ll'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define i32 @f(i32* noalias %S, i32* noalias %B) #0 {
entry:
  %sum.06.reg2mem = alloca i32
  %sum.14.reg2mem = alloca i32
  %sum.22.reg2mem = alloca i32
  br label %entry.split

entry.split:                                      ; preds = %entry
  store i32 0, i32* %sum.06.reg2mem
  br label %for.cond1.preheader

for.cond1.preheader:                              ; preds = %entry.split, %for.inc17
  %indvars.iv65 = phi i64 [ 0, %entry.split ], [ %6, %for.inc17 ]
  %sum.06.reload = load i32* %sum.06.reg2mem
  store i32 %sum.06.reload, i32* %sum.14.reg2mem
  br label %for.cond4.preheader

for.cond4.preheader:                              ; preds = %for.cond1.preheader, %for.inc14
  %indvars.iv23 = phi i64 [ 0, %for.cond1.preheader ], [ %4, %for.inc14 ]
  %sum.14.reload = load i32* %sum.14.reg2mem
  %0 = add i64 %indvars.iv65, %indvars.iv23
  %arrayidx9 = getelementptr i32* %S, i64 %indvars.iv23
  store i32 %sum.14.reload, i32* %sum.22.reg2mem
  br label %for.body6

for.body6:                                        ; preds = %for.cond4.preheader, %for.body6
  %indvars.iv1 = phi i64 [ 0, %for.cond4.preheader ], [ %2, %for.body6 ]
  %sum.22.reload = load i32* %sum.22.reg2mem
  %1 = add i64 %0, %indvars.iv1
  %arrayidx = getelementptr i32* %B, i64 %1
  %tmp10 = load i32* %arrayidx, align 4
  %tmp11 = load i32* %arrayidx9, align 4
  %add10 = add nsw i32 %tmp11, %tmp10
  store i32 %add10, i32* %arrayidx9, align 4
  %add13 = add nsw i32 %tmp10, %sum.22.reload
  %2 = add nuw nsw i64 %indvars.iv1, 1
  %exitcond7 = icmp eq i64 %2, 100
  store i32 %add13, i32* %sum.22.reg2mem
  br i1 %exitcond7, label %for.inc14, label %for.body6

for.inc14:                                        ; preds = %for.body6
  %3 = load i32* %sum.22.reg2mem
  %4 = add nuw nsw i64 %indvars.iv23, 1
  %exitcond = icmp eq i64 %4, 100
  store i32 %3, i32* %sum.14.reg2mem
  br i1 %exitcond, label %for.inc17, label %for.cond4.preheader

for.inc17:                                        ; preds = %for.inc14
  %5 = load i32* %sum.14.reg2mem
  %6 = add nuw nsw i64 %indvars.iv65, 1
  %exitcond9 = icmp eq i64 %6, 100
  store i32 %5, i32* %sum.06.reg2mem
  br i1 %exitcond9, label %for.end19, label %for.cond1.preheader

for.end19:                                        ; preds = %for.inc17
  %7 = load i32* %sum.06.reg2mem
  ret i32 %7
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

; Check for 3 reduction accesses; one in the outer and one inner most loop
; CHECK: RA: Reduction Loop: Loop at depth 3
; CHECK: RA: Reduction Loop: Loop at depth 1
; CHECK: RA: Reduction Loop: Loop at depth 3
; CHECK-NOT: RA: Reduction Loop:
