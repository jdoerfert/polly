; RUN: opt %loadPolly -analyze < %s | FileCheck %s
;
;    void jd(int *A, int c, int d) {
;      for (int i = 0; i < 1024; i++)
;        A[i | 2] = A[(i + c) | 2] + A[(i + 3) | 2] + A[(c + d) | 2] +
;                   A[(c - 5) | 2] + A[d | 2] + A[2 | (i - c)] + A[2 | (i + 3)] +
;                   A[2 | (c + d)] + A[2 | (c - 5)] + A[2 | d];
;    }
;
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

define void @jd(i32* %A, i32 %c, i32 %d) {
entry:
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %i.0 = phi i32 [ 0, %entry ], [ %inc, %for.inc ]
  %exitcond = icmp ne i32 %i.0, 1024
  br i1 %exitcond, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %add = add nsw i32 %i.0, %c
  %or = or i32 %add, 2
  %idxprom = sext i32 %or to i64
  %arrayidx = getelementptr inbounds i32* %A, i64 %idxprom
  %tmp = load i32* %arrayidx, align 4
  %add1 = add nsw i32 %i.0, 3
  %or2 = or i32 %add1, 2
  %idxprom3 = sext i32 %or2 to i64
  %arrayidx4 = getelementptr inbounds i32* %A, i64 %idxprom3
  %tmp1 = load i32* %arrayidx4, align 4
  %add5 = add nsw i32 %tmp, %tmp1
  %add6 = add nsw i32 %c, %d
  %or7 = or i32 %add6, 2
  %idxprom8 = sext i32 %or7 to i64
  %arrayidx9 = getelementptr inbounds i32* %A, i64 %idxprom8
  %tmp2 = load i32* %arrayidx9, align 4
  %add10 = add nsw i32 %add5, %tmp2
  %sub = add i32 %c, -5
  %or11 = or i32 %sub, 2
  %idxprom12 = sext i32 %or11 to i64
  %arrayidx13 = getelementptr inbounds i32* %A, i64 %idxprom12
  %tmp3 = load i32* %arrayidx13, align 4
  %add14 = add nsw i32 %add10, %tmp3
  %or15 = or i32 %d, 2
  %idxprom16 = sext i32 %or15 to i64
  %arrayidx17 = getelementptr inbounds i32* %A, i64 %idxprom16
  %tmp4 = load i32* %arrayidx17, align 4
  %add18 = add nsw i32 %add14, %tmp4
  %sub19 = sub nsw i32 %i.0, %c
  %or20 = or i32 %sub19, 2
  %idxprom21 = sext i32 %or20 to i64
  %arrayidx22 = getelementptr inbounds i32* %A, i64 %idxprom21
  %tmp5 = load i32* %arrayidx22, align 4
  %add23 = add nsw i32 %add18, %tmp5
  %add24 = add nsw i32 %i.0, 3
  %or25 = or i32 %add24, 2
  %idxprom26 = sext i32 %or25 to i64
  %arrayidx27 = getelementptr inbounds i32* %A, i64 %idxprom26
  %tmp6 = load i32* %arrayidx27, align 4
  %add28 = add nsw i32 %add23, %tmp6
  %add29 = add nsw i32 %c, %d
  %or30 = or i32 2, %add29
  %idxprom31 = sext i32 %or30 to i64
  %arrayidx32 = getelementptr inbounds i32* %A, i64 %idxprom31
  %tmp7 = load i32* %arrayidx32, align 4
  %add33 = add nsw i32 %add28, %tmp7
  %sub34 = add i32 %c, -5
  %or35 = or i32 2, %sub34
  %idxprom36 = sext i32 %or35 to i64
  %arrayidx37 = getelementptr inbounds i32* %A, i64 %idxprom36
  %tmp8 = load i32* %arrayidx37, align 4
  %add38 = add nsw i32 %add33, %tmp8
  %or39 = or i32 2, %d
  %idxprom40 = sext i32 %or39 to i64
  %arrayidx41 = getelementptr inbounds i32* %A, i64 %idxprom40
  %tmp9 = load i32* %arrayidx41, align 4
  %add42 = add nsw i32 %add38, %tmp9
  %or43 = or i32 2, %i.0
  %idxprom44 = sext i32 %or43 to i64
  %arrayidx45 = getelementptr inbounds i32* %A, i64 %idxprom44
  store i32 %add42, i32* %arrayidx45, align 4
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %inc = add nsw i32 %i.0, 1
  br label %for.cond

for.end:                                          ; preds = %for.cond
  ret void
}
