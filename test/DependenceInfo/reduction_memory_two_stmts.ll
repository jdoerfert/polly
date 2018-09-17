; RUN: opt %loadPolly -polly-dependence -analyze < %s | FileCheck %s
;
;    void f(int *A) {
;      for (int j = 0; j < 555; j++)
;        for (int i = 0; i < 1000; i++) {
;          A[i] += i;
;          // split
;          A[i] += i;
;        }
;    }
;
; CHECK:	RAW dependences:
; CHECK:		{  }
; CHECK:	WAR dependences:
; CHECK:		{  }
; CHECK:	WAW dependences:
; CHECK:		{  }
; CHECK:	Reduction dependences:
; CHECK:		{ Stmt4[i0, i1] -> Stmt5[i0, i1] : 0 <= i0 <= 554 and 0 <= i1 <= 999; Stmt5[i0, i1] -> Stmt4[1 + i0, i1] : 0 <= i0 <= 553 and 0 <= i1 <= 999 }
;
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

define dso_local void @f(i32* %A) {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc8, %entry
  %j.0 = phi i32 [ 0, %entry ], [ %inc9, %for.inc8 ]
  %exitcond1 = icmp ne i32 %j.0, 555
  br i1 %exitcond1, label %for.body, label %for.cond.cleanup

for.cond.cleanup:                                 ; preds = %for.cond
  br label %for.end10

for.body:                                         ; preds = %for.cond
  br label %for.cond1

for.cond1:                                        ; preds = %for.inc, %for.body
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc ], [ 0, %for.body ]
  %exitcond = icmp ne i64 %indvars.iv, 1000
  br i1 %exitcond, label %for.body4, label %for.cond.cleanup3

for.cond.cleanup3:                                ; preds = %for.cond1
  br label %for.end

for.body4:                                        ; preds = %for.cond1
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 %indvars.iv
  %tmp = load i32, i32* %arrayidx, align 4
  %tmp2 = trunc i64 %indvars.iv to i32
  %add = add nsw i32 %tmp, %tmp2
  store i32 %add, i32* %arrayidx, align 4
  br label %for.body4.split

for.body4.split:
  %arrayidx6 = getelementptr inbounds i32, i32* %A, i64 %indvars.iv
  %tmp3 = load i32, i32* %arrayidx6, align 4
  %tmp4 = trunc i64 %indvars.iv to i32
  %add7 = add nsw i32 %tmp3, %tmp4
  store i32 %add7, i32* %arrayidx6, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body4
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  br label %for.cond1

for.end:                                          ; preds = %for.cond.cleanup3
  br label %for.inc8

for.inc8:                                         ; preds = %for.end
  %inc9 = add nuw nsw i32 %j.0, 1
  br label %for.cond

for.end10:                                        ; preds = %for.cond.cleanup
  ret void
}
