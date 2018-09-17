; RUN: opt %loadPolly -polly-dependence -analyze < %s | FileCheck %s
;
; CHECK:     Reduction dependences:
; CHECK-DAG: Stmt_for_cond2[i0] -> Stmt_for_cond2[1 + i0] : 0 < i0 <= 99;
; CHECK-DAG: Stmt_for_inc9[i0] -> Stmt_for_inc9[1 + i0] : 0 <= i0 <= 98;
; CHECK-DAG: Stmt_for_cond[i0] -> Stmt_for_inc[i0] : 0 < i0 <= 99;
; CHECK-DAG: Stmt_for_cond2[i0] -> Stmt_for_inc9[i0] : 0 < i0 <= 99;
; CHECK-DAG: Stmt_for_cond[i0] -> Stmt_for_cond[1 + i0] : 0 < i0 <= 99;
; CHECK-DAG: Stmt_for_inc[i0] -> Stmt_for_inc[1 + i0] : 0 <= i0 <= 98
;
;    int f(int *A) {
;      int s = 0;
;      for (int i = 0; i < 100; i++)
;        s += A[i];
;      for (int i = 0; i < 100; i++)
;        s += A[i];
;      return s;
;    }
;
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

define i32 @f(i32* %A) {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %indvars.iv1 = phi i64 [ %indvars.iv.next2, %for.inc ], [ 0, %entry ]
  %s.0 = phi i32 [ 0, %entry ], [ %add, %for.inc ]
  %exitcond3 = icmp ne i64 %indvars.iv1, 100
  br i1 %exitcond3, label %for.body, label %for.cond.cleanup

for.cond.cleanup:                                 ; preds = %for.cond
  %s.0.lcssa = phi i32 [ %s.0, %for.cond ]
  br label %for.end

for.body:                                         ; preds = %for.cond
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %arrayidx = getelementptr inbounds i32, i32* %A, i64 %indvars.iv1
  %tmp = load i32, i32* %arrayidx, align 4
  %add = add nsw i32 %s.0, %tmp
  %indvars.iv.next2 = add nuw nsw i64 %indvars.iv1, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond.cleanup
  br label %for.cond2

for.cond2:                                        ; preds = %for.inc9, %for.end
  %indvars.iv = phi i64 [ %indvars.iv.next, %for.inc9 ], [ 0, %for.end ]
  %s.1 = phi i32 [ %s.0.lcssa, %for.end ], [ %add8, %for.inc9 ]
  %exitcond = icmp ne i64 %indvars.iv, 100
  br i1 %exitcond, label %for.body5, label %for.cond.cleanup4

for.cond.cleanup4:                                ; preds = %for.cond2
  %s.1.lcssa = phi i32 [ %s.1, %for.cond2 ]
  br label %for.end11

for.body5:                                        ; preds = %for.cond2
  br label %for.inc9

for.inc9:                                         ; preds = %for.body5
  %arrayidx7 = getelementptr inbounds i32, i32* %A, i64 %indvars.iv
  %tmp4 = load i32, i32* %arrayidx7, align 4
  %add8 = add nsw i32 %s.1, %tmp4
  %indvars.iv.next = add nuw nsw i64 %indvars.iv, 1
  br label %for.cond2

for.end11:                                        ; preds = %for.cond.cleanup4
  ret i32 %s.1.lcssa
}
