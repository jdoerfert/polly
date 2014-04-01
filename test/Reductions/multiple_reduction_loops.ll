; RUN: opt %loadPolly -basicaa -analyze -polly-basic-ri -polly-collect-reductions < %s | FileCheck %s
; ModuleID = 'multiple-reduction-loops.ll'
;
; void f(int * __restrict__ S, int * __restrict__ B) {
;   int i, j, k;
;   for (i = 0; i < 100; i++) {
;     for (j = 0; j < 100; j++) {
;       for (k = 0; k < 100; k++) {
;         S[j] += B[i+j+k];
;       }
;     }
;   }
; }

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define void @f(i32* noalias %S, i32* noalias %B) #0 {
entry:
  br label %entry.split

entry.split:                                      ; preds = %entry
  br label %for.cond1.preheader

for.cond1.preheader:                              ; preds = %entry.split, %for.inc14
  %indvars.iv63 = phi i64 [ 0, %entry.split ], [ %4, %for.inc14 ]
  br label %for.cond4.preheader

for.cond4.preheader:                              ; preds = %for.cond1.preheader, %for.inc11
  %indvars.iv22 = phi i64 [ 0, %for.cond1.preheader ], [ %3, %for.inc11 ]
  %0 = add i64 %indvars.iv63, %indvars.iv22
  %arrayidx9 = getelementptr i32* %S, i64 %indvars.iv22
  br label %for.body6

for.body6:                                        ; preds = %for.cond4.preheader, %for.body6
  %indvars.iv1 = phi i64 [ 0, %for.cond4.preheader ], [ %2, %for.body6 ]
  %1 = add i64 %0, %indvars.iv1
  %arrayidx = getelementptr i32* %B, i64 %1
  %tmp10 = load i32* %arrayidx, align 4
  %tmp11 = load i32* %arrayidx9, align 4
  %add10 = add nsw i32 %tmp11, %tmp10
  store i32 %add10, i32* %arrayidx9, align 4
  %2 = add nuw nsw i64 %indvars.iv1, 1
  %exitcond4 = icmp eq i64 %2, 100
  br i1 %exitcond4, label %for.inc11, label %for.body6

for.inc11:                                        ; preds = %for.body6
  %3 = add nuw nsw i64 %indvars.iv22, 1
  %exitcond = icmp eq i64 %3, 100
  br i1 %exitcond, label %for.inc14, label %for.cond4.preheader

for.inc14:                                        ; preds = %for.inc11
  %4 = add nuw nsw i64 %indvars.iv63, 1
  %exitcond5 = icmp eq i64 %4, 100
  br i1 %exitcond5, label %for.end16, label %for.cond1.preheader

for.end16:                                        ; preds = %for.inc14
  ret void
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

; Check for 2 reduction accesses; one in the outer and one inner most loop
; CHECK: RA: Reduction Loop: Loop at depth 1
; CHECK: RA: Reduction Loop: Loop at depth 3
; CHECK-NOT: RA: Reduction Loop: Loop at depth 2
