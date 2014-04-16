; RUN: opt %loadPolly -polly-collect-reductions -polly-basic-ri -analyze -basicaa < %s | FileCheck %s
;
; void f(int *__restrict__ S, int *__restrict__ B, int *__restrict__ sum) {
;   int i, j, k;
;   for (i = 0; i < 128; i++) {
;     for (j = 0; j < 256; j++) {
;       for (k = 0; k < 512; k++) {
;         S[j] += B[i + j + k];
;         sum[i] += B[i];
;       }
;     }
;   }
; }
;
; ModuleID = 'multiple_reduction_loops_multiple_reductions_2.ll'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define void @f(i32* noalias %S, i32* noalias %B, i32* noalias %sum) #0 {
entry:
  br label %entry.split

entry.split:                                      ; preds = %entry
  br label %for.cond1.preheader

for.cond1.preheader:                              ; preds = %entry.split, %for.inc19
  %indvars.iv63 = phi i64 [ 0, %entry.split ], [ %4, %for.inc19 ]
  %arrayidx12 = getelementptr i32* %B, i64 %indvars.iv63
  %arrayidx14 = getelementptr i32* %sum, i64 %indvars.iv63
  br label %for.cond4.preheader

for.cond4.preheader:                              ; preds = %for.cond1.preheader, %for.inc16
  %indvars.iv22 = phi i64 [ 0, %for.cond1.preheader ], [ %3, %for.inc16 ]
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
  %tmp12 = load i32* %arrayidx12, align 4
  %tmp13 = load i32* %arrayidx14, align 4
  %add15 = add nsw i32 %tmp13, %tmp12
  store i32 %add15, i32* %arrayidx14, align 4
  %2 = add nuw nsw i64 %indvars.iv1, 1
  %exitcond4 = icmp eq i64 %2, 512
  br i1 %exitcond4, label %for.inc16, label %for.body6

for.inc16:                                        ; preds = %for.body6
  %3 = add nuw nsw i64 %indvars.iv22, 1
  %exitcond = icmp eq i64 %3, 256
  br i1 %exitcond, label %for.inc19, label %for.cond4.preheader

for.inc19:                                        ; preds = %for.inc16
  %4 = add nuw nsw i64 %indvars.iv63, 1
  %exitcond5 = icmp eq i64 %4, 128
  br i1 %exitcond5, label %for.end21, label %for.cond1.preheader

for.end21:                                        ; preds = %for.inc19
  ret void
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

; Check for 4 reduction accesses
; CHECK: RA: Reduction Loop: Loop at depth 1
; CHECK: RA: Reduction Loop: Loop at depth 3
; CHECK: RA: Reduction Loop: Loop at depth 2
; CHECK: RA: Reduction Loop: Loop at depth 3
; CHECK-NOT: RA: Reduction Loop:
