; 1 tryte = 2 byte+2bits
; FILE END errorneus BCT 11111111,1_0000000,01_000000 (pos:111111111,neg:000000001)
postpone
        db padbyte or (-1 shl padshift),$FF shr (7-padshift),2 shl padshift
end postpone

padshift = 0
padbyte = 0

macro extract_components value,outpos:0,outneg:0,size:0
        local Number,Carry,Digit,?topos
        if value
                Number=value
                if Number<0
                        ?topos = -1
                        Number = -Number
                else
                        ?topos = 1
                end if
                Carry=0
                while Number
                        if size&size*9<%
                                err value couldn`t fit
                        end if
                        Digit = Carry + Number mod 3
                        Number = Number / 3
                        if Digit < 2
                                Carry = 0
                        else
                                Digit = Digit - 3
                                Carry = 1
                        end if
                        if Digit = ?topos
                                outpos = outpos or 1 shl (%-1)
                        else if Digit = -?topos
                                outneg = outneg or 1 shl (%-1)
                        end if
                        if ~Number
                                if Carry = ?topos
                                        outpos = outpos or 1 shl %
                                else if Carry = -?topos
                                        outneg = outneg or 1 shl %
                                end if
                        end if
                end while
        end if
end macro

macro tryte value
        local v,pos,neg,block,idx,size
        v = value
        pos = 0
        neg = 0
        extract_components v,pos,neg,1
        db (padbyte + (pos shl padshift)) and $FF,(pos shr (7-padshift) + neg shl (1+padshift)) and $FF
        if padshift = 6
                db (neg shr (7-padshift)) and $FF
                padshift = 0
                padbyte = 0
        else
                padshift = padshift + 2
                padbyte = (neg shr (7-padshift)) and $FF
        end if
end macro



macro dt? definitions&
    iterate value,definitions
        match n =dup? v, value
            repeat n
                tryte v
            end repeat
        else
            tryte value
        end match
    end iterate
end macro

struc dt? definitions&
    label . : byte
    iterate value,definitions
        match n =dup? v, value
            repeat n
                tryte v
            end repeat
        else
            tryte value
        end match
    end iterate
end struc

macro jmp addr
        dt addr*81
end macro

macro jz addr
        dt 3+addr*81
end macro

macro jn addr
        dt 2+addr*81
end macro

macro jp addr
        dt 4+addr*81
end macro

macro ld reg,val
        match ,val
                dt 9+reg*81
        else match =S,reg
                dt 9+val*81
        else match =R,reg
                dt 7+val*81
        else match =F,reg
                dt -3+val*81
        end match
end macro

macro add reg,val
        match ,val
                dt 10+reg*81
        else match =S,reg
                dt 10+val*81
        else match =F,reg
                dt -4+val*81
        end match
end macro

macro rel val
        dt -2+val*81
end macro

macro sub reg,val
        match ,val
                dt 8+reg*81
        else match =S,reg
                dt 8+val*81
        end match
end macro

macro mov op1,op2
        match [val],op1
                match ,op2
                        dt  9+val*81 ;+00 ; send to S; mov S,[imm] ; mov [imm]
                else match =F,op2
                        dt -1+val*81 ;00- ; get from F; mov [imm],F
                else match =C,op2
                        dt  1+val*81 ;00+ ; get from C; mov [imm],C
                else match =S,op2
                        dt -5+val*81 ;-++ ; get from S; mov [imm],S
                else
                        err 'invalid operand'
                end if
        else match [val],op2
                match =F,op1
                        dt -3+val*81 ;0-0 ; send to F; mov F,[imm]
                else match =R,op1
                        dt  7+val*81 ;+-+ ; send to R; mov R,[imm]
                else match =S,op1
                        dt  9+val*81 ;+00 ; send to S; mov S,[imm] ; mov [imm]
                else
                        err 'invalid combination of operands'
                end if
        else
                err 'invalid combination of operands'
        end if
end macro

;setun ternary opcodes
UD1             =-13
UD2             =-12
UD3             =-11
MBread          =-10
IO              =-9
MBwrite         =-8
Snorm           =-7
Sshift          =-6
Ssave           =-5
Fadd            =-4
Fload           =-3
Frel            =-2
Fsave           =-1
Jump            = 0
Link            = 1
JumpNeg         = 2
JumpZero        = 3
JumpPos         = 4
Halt            = 5
Sproduct        = 6
Rload           = 7
Ssub            = 8
Sload           = 9
Sadd            = 10
Smuladd         = 11
Smul            = 12
Saddscaled      = 13

;macro jmp_instr [name,opcode]
;        macro name addr
;                ;not all addresses allowed, test them
;                dt opcode+addr*81
;        end macro
;end macro

;jmp_instr jmp,Jump,\
;          jz,JumpZero,\
;          jn,JumpNeg,\
;          jp,JumpPos

macro ter_instr opcode,param
        dt opcode+param*81
end macro

macro add op1,op2,op3
        match ,op3
                match ,op2
                        match [val],op1
                                dt 10+val*81 ; +0+ ; sum to S; add S,[imm]; add [imm]
                        else match =R*[val],op1
                                dt 13+val*81 ; +++ ; scaled sum to S; add S,R*[imm]; add S,[imm]*R; add R*[imm]; add [imm]*R
                        else match [val]*=R,op1
                                dt 13+val*81 ; +++ ; scaled sum to S; add S,R*[imm]; add S,[imm]*R; add R*[imm]; add [imm]*R
                        else
                                err 'invalid combination of operands'
                        end if
                else match [val],op2
                        match =S,op1
                                dt 10+val*81 ; +0+ ; sum to S; add S,[imm]; add [imm]
                        else match =S*=R,op1
                                err 'invalid combination of operands'
                        else
                                err 'invalid combination of operands'
                        end if
end macro

; выше команды на выполнение непосредственно компилятору (макросы и прочее - все то что формирует выходной формат и синтаксис)
; ниже команды на языке (теперь) доступного синтаксиса, которые непосредственно формируют (полезное) содержание выходного файла.



dt 3 ; можно определять как троичные данные

jmp 5; так и инструкции

