;*******************************************************************************
;@file				 Main.s
;@project		     Microprocessor Systems Term Project
;@date
;
;@PROJECT GROUP
;@groupno	7
;@member1	Ece Nur SEN - 15150104
;@member2	Dilara INAN - 150150109
;@member3	Idil SEZGIN - 150150015
;@member4	Muhammed Enes DENIZ - 070170450
;*******************************************************************************
;*******************************************************************************
;@section 		INPUT_DATASET
;*******************************************************************************

;@brief 	This data will be used for insertion and deletion operation.
;@note		The input dataset will be changed at the grading. 
;			Therefore, you shouldn't use the constant number size for this dataset in your code. 
				AREA     IN_DATA_AREA, DATA, READONLY
IN_DATA			DCD		0x10, 0x20, 0x15, 0x65, 0x25, 0x01, 0x01, 0x12, 0x65, 0x25, 0x85, 0x46, 0x10, 0x00
END_IN_DATA

;@brief 	This data contains operation flags of input dataset. 
;@note		0 -> Deletion operation, 1 -> Insertion 
				AREA     IN_DATA_FLAG_AREA, DATA, READONLY
IN_DATA_FLAG	DCD		0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x00, 0x02
END_IN_DATA_FLAG


;*******************************************************************************
;@endsection 	INPUT_DATASET
;*******************************************************************************

;*******************************************************************************
;@section 		DATA_DECLARATION
;*******************************************************************************

;@brief 	This part will be used for constant numbers definition.
NUMBER_OF_AT	EQU		20									; Number of Allocation Table
AT_SIZE			EQU		NUMBER_OF_AT*4						; Allocation Table Size


DATA_AREA_SIZE	EQU		AT_SIZE*32*2						; Allocable data area
															; Each allocation table has 32 Cell
															; Each Cell Has 2 word (Value + Address)
															; Each word has 4 byte
ARRAY_SIZE		EQU		AT_SIZE*32							; Allocable data area
															; Each allocation table has 32 Cell
															; Each Cell Has 1 word (Value)
															; Each word has 4 byte
LOG_ARRAY_SIZE	EQU     AT_SIZE*32*3						; Log Array Size
															; Each log contains 3 word
															; 16 bit for index
															; 8 bit for error_code
															; 8 bit for operation
															; 32 bit for data
															; 32 bit for timestamp in us

;//-------- <<< USER CODE BEGIN Constant Numbers Definitions >>> ----------------------															
							


;//-------- <<< USER CODE END Constant Numbers Definitions >>> ------------------------	

;*******************************************************************************
;@brief 	This area will be used for global variables.
				AREA     GLOBAL_VARIABLES, DATA, READWRITE
				ALIGN	
TICK_COUNT		SPACE	 4									; Allocate #4 byte area to store tick count of the system tick timer.
FIRST_ELEMENT  	SPACE    4									; Allocate #4 byte area to store the first element pointer of the linked list.
INDEX_INPUT_DS  SPACE    4									; Allocate #4 byte area to store the index of input dataset.
INDEX_ERROR_LOG SPACE	 4									; Allocate #4 byte aret to store the index of the error log array.
PROGRAM_STATUS  SPACE    4									; Allocate #4 byte to store program status.
															; 0-> Program started, 1->Timer started, 2-> All data operation finished.
;//-------- <<< USER CODE BEGIN Global Variables >>> ----------------------															
							


;//-------- <<< USER CODE END Global Variables >>> ------------------------															

;*******************************************************************************

;@brief 	This area will be used for the allocation table
				AREA     ALLOCATION_TABLE, DATA, READWRITE		
				ALIGN	
__AT_Start
AT_MEM       	SPACE    AT_SIZE							; Allocate #AT_SIZE byte area from memory.
__AT_END

;@brief 	This area will be used for the linked list.
				AREA     DATA_AREA, DATA, READWRITE		
				ALIGN	
__DATA_Start
DATA_MEM        SPACE    DATA_AREA_SIZE						; Allocate #DATA_AREA_SIZE byte area from memory.
__DATA_END

;@brief 	This area will be used for the array. 
;			Array will be used at the end of the program to transform linked list to array.
				AREA     ARRAY_AREA, DATA, READWRITE		
				ALIGN	
__ARRAY_Start
ARRAY_MEM       SPACE    ARRAY_SIZE						; Allocate #ARRAY_SIZE byte area from memory.
__ARRAY_END

;@brief 	This area will be used for the error log array. 
				AREA     ARRAY_AREA, DATA, READWRITE		
				ALIGN	
__LOG_Start
LOG_MEM       	SPACE    LOG_ARRAY_SIZE						; Allocate #DATA_AREA_SIZE byte area from memory.
__LOG_END

;//-------- <<< USER CODE BEGIN Data Allocation >>> ----------------------															
							


;//-------- <<< USER CODE END Data Allocation >>> ------------------------															

;*******************************************************************************
;@endsection 	DATA_DECLARATION
;*******************************************************************************

;*******************************************************************************
;@section 		MAIN_FUNCTION
;*******************************************************************************

			
;@brief 	This area contains project codes. 
;@note		You shouldn't change the main function. 				
				AREA MAINFUNCTION, CODE, READONLY
				ENTRY
				THUMB
				ALIGN 
__main			FUNCTION
				EXPORT __main
				BL	Clear_Alloc					; Call Clear Allocation Function.
				BL  Clear_ErrorLogs				; Call Clear ErrorLogs Function.
				BL	Init_GlobVars				; Call Initiate Global Variable Function.
				BL	SysTick_Init				; Call Initialize System Tick Timer Function.
				LDR R0, =PROGRAM_STATUS			; Load Program Status Variable Addresses.
LOOP			LDR R1, [R0]					; Load Program Status Variable.
				CMP	R1, #2						; Check If Program finished.
				BNE LOOP						; Go to loop If program do not finish.
STOP			B	STOP						; Infinite loop.
				
				ENDFUNC
			
;*******************************************************************************
;@endsection 		MAIN_FUNCTION
;*******************************************************************************				

;*******************************************************************************
;@section 			USER_FUNCTIONS
;*******************************************************************************

;@brief 	This function will be used for System Tick Handler
SysTick_Handler	FUNCTION			
;//-------- <<< USER CODE BEGIN System Tick Handler >>> ----------------------
				EXPORT	SysTick_Handler
				push {LR}					; push return address to stack
				push {r0-r6} 				; storing old values of registers
				LDR R6,=TICK_COUNT			; getting the address of tick_count
				LDR R5,=IN_DATA 			; getting the address of input_data
				LDR R4,= IN_DATA_FLAG 		; getting the address of input_data_flag
				LDR R6,[R6]					; R6, value of tick_count
				LDR R0,[R5,R6]				; R0, reading input data
				LDR R2,[R4,R6]				; R2, flag of read data / operation
				CMP R2,#0					; if flag == 0
				BNE InsertJump				; call remove function
				BL	Remove					; call remove function
InsertJump		CMP R2,#1					; if flag == 1
				BNE LL2ArrJump				; call insert function
				BL	Insert					; branch to insert function
LL2ArrJump		CMP R2,#2					; if flag == 2
				BNE	Jumpto					; if not equal branch to jumpto
				BL LinkedList2Arr			; call linked list to array function
Jumpto			MOV R1,R0					; R1 = error code, R0 contains return value of functions above
				MOV R0,R6					; R0, index / value of tick_count
				LDR R3,[R5,R6]				; R3, data / last read data
				BL WriteErrorLog			; call for write error log function
				ADDS R6,R6,#4				; increment tick_count
				LDR R7,=TICK_COUNT			; getting the address of tick_count
				STR R6,[R7]					; store new tick count in global variable TICK_COUNT
				LDR R3,=END_IN_DATA			; get the ending address of input_data array
				ADDS R6,R6,R5				; get address of where the cursor input_data is
				CMP R3,R6					; if we read all data in input_data array
				BNE SysStopJump				; call remove function
				BL SysTick_Stop				; stop timer
SysStopJump		pop {r0-r6}					; replacing old values of the registers
				pop {r7}					; pop return address from stack
				BX	r7						; return
;//-------- <<< USER CODE END System Tick Handler >>> ------------------------				
				ENDFUNC

;*******************************************************************************				

;@brief 	This function will be used to initiate System Tick Handler
SysTick_Init	FUNCTION			
;//-------- <<< USER CODE BEGIN System Tick Timer Initialize >>> ----------------------
				push {r0-r7}				; storing old values of registers
				LDR R0,= 0xE000E010			; storing address of systick control and status register
				LDR R1,=6615				; RELOAD value of systick
				STR R1, [R0,#4] 			; storing RELOAD value to systick reload value register
				MOVS R1,#0					; 0, current value of systick counter
				STR R1,[R0,#8]				; setting current value of systick counter
				MOVS R1,#7					; CLKSOURCE=1 TICKINT=1 ENABLE=1
				STR R1,[R0]					; setting 111 to systick control and statur register
				LDR R7,= PROGRAM_STATUS		; R7=address of program status global variable
				MOVS R6,#1					; 1 = timer started
				STR R6,[R7]					; setting program status to timer started
				pop {r0-r7}					; replacing old values of the registers
				BX LR						; return
;//-------- <<< USER CODE END System Tick Timer Initialize >>> ------------------------				
				ENDFUNC

;*******************************************************************************				

;@brief 	This function will be used to stop the System Tick Timer
SysTick_Stop	FUNCTION			
;//-------- <<< USER CODE BEGIN System Tick Timer Stop >>> ----------------------
				push {LR}					;push return address to stack
				push {r0-r6} 				; storing old values of registers
				LDR R0,= 0xE000E010			; address of systick control and status register
				MOVS R1,#0					; COUNTFLAG = 0 CLKSOURCE=0 TICKINT=0 ENABLE=0
				STR R1,[R0]					; stopping timer
				STR	R1,[R0,#8]				; clearing interrupt flag
				LDR R7,= PROGRAM_STATUS		; R7=address of program status global variable
				MOVS R6,#2					; 2 = all data operations finished
				STR R6,[R7]					; setting program status to all data operations finished
				pop {r0-r6}  				; replacing old values of the registers
				pop {r7}					; pop return address from stack
				BX	r7 						; return
;//-------- <<< USER CODE END System Tick Timer Stop >>> ------------------------				
				ENDFUNC

;*******************************************************************************				

;@brief 	This function will be used to clear allocation table
Clear_Alloc		FUNCTION			
;//-------- <<< USER CODE BEGIN Clear Allocation Table Function >>> ----------------------															
			PUSH	{r0-r7}			;storing old values of registers
			LDR		r5,=AT_MEM		;r5 = AT_MEM[0]
			MOVS	r0,#0			;index		
			MOVS	r1,#AT_SIZE		;total size
chckindx	CMP		r1,r0			;check for remaining bits
			BEQ		endClrAllc		;if there is not bit remaning end this function
			MOVS	r2,#0			;R2=0
			STR		r2,[r5,r0]		;clear selected index
			ADDS	r0,#4			;index++
			B		chckindx		;check index
endClrAllc	POP 	{r0-r7}			;end function
			BX		LR				;return
;//-------- <<< USER CODE END Clear Allocation Table Function >>> ------------------------				
				ENDFUNC
				
;*******************************************************************************		

;@brief 	This function will be used to clear error log array
Clear_ErrorLogs	FUNCTION			
;//-------- <<< USER CODE BEGIN Clear Error Logs Function >>> ----------------------															
			PUSH	{r0-r7}				;storing old values of registers
			LDR		r5,=LOG_MEM			;r5 = LOG_MEM[0]
			MOVS	r0,#0				;index		
			LDR		r1,=LOG_ARRAY_SIZE	;total size
chckindxe	CMP		r1,r0				;check for remaining bits
			BEQ		endClrErr			;if there is no bit remaining end this function
			MOVS	r2,#0				;R2 = 0
			LDR		r2,[r5,r0]			;clear selected index
			ADDS	r0,#4				;index++
			B		chckindxe			;check index
endClrErr	POP 	{r0-r7}				;end function
			BX		LR					;return
;//-------- <<< USER CODE END Clear Error Logs Function >>> ------------------------				
				ENDFUNC
				
;*******************************************************************************

;@brief 	This function will be used to initialize global variables
Init_GlobVars	FUNCTION			
;//-------- <<< USER CODE BEGIN Initialize Global Variables >>> ----------------------															
				PUSH	{r0-r7}					; storing old values of registers
				MOVS 	R7,#0					; initial value
				LDR		R6,=TICK_COUNT			; getting the address of tick_count
				STR		R7,[R6]					; initializing the tick_count value
				LDR		R6,=FIRST_ELEMENT		; getting the address of first_element
				STR		R7,[R6]					; initializing the first_element value
				LDR		R6,=INDEX_INPUT_DS		; getting the address of index_input_ds
				STR		R7,[R6]					; initializing the index_input_ds value
				LDR		R6,=INDEX_ERROR_LOG		; getting the address of index_error_log
				STR		R7,[R6]					; initializing the index_error_log value
				LDR		R6,=PROGRAM_STATUS		; getting the address of program_status
				STR		R7,[R6]					; initializing the program_status value
				POP 	{r0-r7}					; replacing old values of the registers
				BX 		LR						; return
;//-------- <<< USER CODE END Initialize Global Variables >>> ------------------------				
				ENDFUNC
				
;*******************************************************************************	

;@brief 	This function will be used to allocate the new cell 
;			from the memory using the allocation table.
;@return 	R0 <- The allocated area address
Malloc			FUNCTION			
;//-------- <<< USER CODE BEGIN System Tick Handler >>> ----------------------
				PUSH	{LR}			; push return address to stack
				PUSH	{r1-r6}			;storing old values of registers
				MOVS 	R6,#1			;1 value for comparison
				MOVS	R3,#0			;0 value for comparison
				LDR 	R0,=AT_MEM		;Load allocation table start address
				LDR		R4,= __AT_END	;Load allocation table end address
mlocbegin		MOVS	R5,#0			;counter for cell number
				LDR 	R2,=0X1			;initial value for comparison
mlocloop		LDR		R1,[R0]			;Load the current content of the allocation table
				ANDS	R1,R1,R2		;bitwise and operation to find the current avaiable lowest bit
				CMP		R1,R3			;if the result 0 that means we found the current lowest one
				BEQ		mlocreturn		;if r1 = r3 found the 0 bit go and return the address of it otherwise countinue
				LSLS	R2,R2,#1		;ready the comparison value for looking the next bit 001 -> 010
				ADDS	R5,R5,#1		;increase the cell number by 1
				CMP		R2,R3			;if we reached 0 that means there is no space in this word
				BNE		mlocloop		;if we did not reach the end of the word (32-bit) repeat the process otherwise continue
				ADDS 	R0,R0,#4		;look for the next word
				CMP 	R0,R4			;check whether we reached the end of the allocation table 
				BNE		mlocbegin		;if we did not reach go to the begin otherwise continue
				MOVS	R0,#0			;if we look all of the allocation table and could not any space that means the linkedlist is full
				POP 	{r1-r7}			;replacing old values of the registers
				BX		LR				;go back
mlocreturn		LDR		R1,[R0]			;Load the current content of the allocation table
				ORRS	R1,R1,R2		;set the found cell location as 1 e.g. we found at 1th bit so 001 -> 011
				STR		R1,[R0]			;store it back in the allocation table
				LDR 	R1,=AT_MEM		;load the allocation table start address
				SUBS	R0,R0,R1		;&allocation_table[current_looking_word] - &allocation_table[0] will give as the row value*4
				LSRS	R0,R0,#2		;divide it to acquire row value in other words the current word we're looking for e.g. 1st word. there exists 20 word
				MOVS	R4,#32			;load the maximum value of the cell
				MULS	R0,R4,R0		;row * maximum cell value
				ADDS	R0,R0,R5		;(row * maximum cell value) + current cell index
				LSLS	R0,#3			;(row * maximum cell value) + current cell index x 2^3 to convert locate it place in data_mem
				LDR		R5,=DATA_MEM	;Allocable data area address
				ADDS	R0,R5,R0		;&data_mem + ((current_row x max_cell_number) + current_cell_index) x 2^3)
				POP 	{r1-r6}			;replacing old values of the registers
				POP		{R7}			;pop return address from stack
				BX 		R7				;go back
;//-------- <<< USER CODE END System Tick Handler >>> ------------------------				
				ENDFUNC
				
;*******************************************************************************				

;@brief 	This function will be used for deallocate the existing area
;@param		R0 <- Address to deallocate
Free			FUNCTION			
;//-------- <<< USER CODE BEGIN Free Function >>> ----------------------
				PUSH	{LR}			;push return address to stack
				PUSH	{r1-r6}			;storing old values of registers
				LDR		R1,=DATA_MEM	;load the start address of the data_mem
				MOVS	R6,#0			;0 value for comparison
				MOVS	R2,#0			;r2 will be used the locate the row
				MOVS	R5,#2			;r5 = 2 later will used for calculation
				LDR 	R4,=AT_MEM		;Load allocation table start addres
				LDR		R3,=256			;the maximum cell number
				SUBS 	R0,R0,R1		;R0 = &data_mem[i] - &data_mem[0]  
FreeLoop		CMP		R0,R3			;if &data_mem[i] - &data_mem[0]> go to freediv otherwise continue
				BGE		FreeDiv			;jump to FreeDiv
				B		FreeMove		;jump to FreeMove
FreeDiv			SUBS	R0,R0,R3		;&data_mem[i] - 256 to reduce it's row value by 1
				ADDS	R2,R2,#4		;increase the row value
				B		FreeLoop		;jump to FreeLoop
FreeMove		ADDS	R4,R4,R2		;locate the word's address which it's bit will be freed
				LSRS	R0,R0,#3		;cell value in terms of at_mem context
				CMP		R0,R6			;if cell value not equal to 0 branch
				BNE		Jump0			;jump to jump0
				MOVS	R5,#1			;set r5 = 1 which will used for bit clearing
				B		FreeClear		;Branch to freeClear subroutine
Jump0			SUBS	R0,R0,#1		;cell value - 1 
				LSLS	R5,R5,R0		;find the bit which will be cleared by 2^R0 or if R0 = 0 1^R0
FreeClear		LDR		R1,[R4]			;Load the current word(in allocation table) content to R1
				BICS	R1,R1,R5		;Set the bit as 0
				STR		R1,[R4]			;Store it back into the allocation table
				POP 	{r1-r6}			;replacing old values of the registers
				POP		{R7}			;pop the rturn address from stack
				BX 		R7				;go back
;//-------- <<< USER CODE END Free Function >>> ------------------------				
				ENDFUNC
				
;*******************************************************************************				

;@brief 	This function will be used to insert data to the linked list
;@param		R0 <- The data to insert
;@return    R0 <- Error Code
Insert			FUNCTION			
;//-------- <<< USER CODE BEGIN Insert Function >>> ----------------------
				PUSH	{LR}					; push return address to stack
				PUSH	{r1-r6}					; storing old values of registers
				MOVS	R7,#0					; store 0 to register 7 for comparisions
				LDR		R6,=FIRST_ELEMENT		; getting the address of the address of the first element of linked list
				LDR		R6,[R6]					; addres of first element 
				MOV		R5,R6					; addres of first element  is in R5, for empty linked list
				CMP		R6,R7					; if the address of the first element of linked list is 0
				BEQ		InsertFirst				; Linked list is empty
				LDR		R5,[R6]					; store the first element of linked list to R5
				CMP 	R5,R0					; if first element <= value
				BLE		InsertLoop				; jump to loop
				CMP 	R5,R0					; if first element > value
				BGT		InsertFirst				; insert value at the start of the linked list
InsertLoop		CMP 	R5,R0					; if value == element
				BEQ		InsertEq				; dublicate in linked list
				MOVS	R2,#4					; immediate value 4 for reaching second word
				ADD		R6,R6,R2				; get the address of the address of the next element
				LDR		R5,[R6]					; load the address of the next element in linked list
				CMP 	R5,R7					; if the address of next element is 0, end of linked list
				BEQ		InsertMid				; add element to end
				LDR		R4,[R5]					; load the value of the next element
				CMP 	R4,R0					; if  element > value
				BGT		InsertMid				; insert value before the element
				MOV		R6,R5					; now the R6 address the value of element
				LDR		R5,[R6]					; R5 is the value of element
				B		InsertLoop				; repeat the loop
InsertFirst		MOV		R1,R0					; save the value to R1, since the malloc will be called
				BL 		Malloc					; call malloc function
				LDR		R7,=0x0					; for comparision 
				CMP		R7,R0					; if malloc returned error
				BEQ 	InsertFull				; the linked list is full
				STR		R6,[R0,#4]				; link the new value + address with the old first element of the linked list
				STR		R1,[R0,#0]				; save the value to address given by malloc
				LDR		R6,=FIRST_ELEMENT		; getting the address of address first element of linked list
				STR		R0,[R6]					; save the address of the new value as first element of the linked list
				MOVS	R0,#0					; insertation is successful
				B 		InsertRet				; go to return subroutine
InsertMid		MOV		R1,R0					; save the value to R1, since the malloc will be called
				BL 		Malloc					; call malloc function
				LDR		R7,=0x0					; for comparision 
				CMP		R7,R0					; if malloc returned error
				BEQ 	InsertFull				; the linked list is full
				STR		R5,[R0,#4]				; save the address of next element in linked list to newly added element
				STR		R1,[R0]					; save the value to address given by malloc
				STR		R0,[R6]					; save the address of the new value as next element's address of the  previous element 
				MOVS	R0,#0					; insertation is successful
				B 		InsertRet				; go to return subroutine
InsertFull		MOVS	R0,#1					; set error code to 1, linked list is full
				B		InsertRet				; go to return subroutine
InsertEq		MOVS	R0,#2					; set error code to 2, dublicate in linked list
				B		InsertRet				; go to return subroutine
InsertRet		POP 	{r1-r6}					; replacing old values of the registers
				POP		{r7}					; pop return address
				BX 		R7						; return			
;//-------- <<< USER CODE END Insert Function >>> ------------------------				
				ENDFUNC
				
;*******************************************************************************				

;@brief 	This function will be used to remove data from the linked list
;@param		R0 <- the data to delete
;@return    R0 <- Error Code
Remove			FUNCTION
;//-------- <<< USER CODE BEGIN Remove Function >>> ----------------------															
				PUSH	    {LR}					;push return address to stack
				PUSH	    {r1-r6}			        ;storing old values of registers
                LDR         r7,=FIRST_ELEMENT       ;getting the address of address first element of linked list
				MOVS        r2,#0                   ;store 0 to register 2 for comparisions
				LDR         r1,=FIRST_ELEMENT       ;getting the address of address first element of linked list
				LDR			R4,[R7]                 ;r4 = first value address of linked list
                CMP         r2,R4                   ;compare first value address of linked list with 0
				BEQ         rmvEmpty                ;if equal linked list is empty
rmvCntrl        LDR         r7,[r7]                 ;if not r7 = address of next element
                LDR         r6,[r7]                 ;r6 = the current value
                CMP         r6,r0                   ;compare current value with input value
                BEQ         rmvFound                ;if equal jump to found
                ADDS        r7,r7,#4                ;if not get the next element
                MOV         r3,r7                   ;r3 = next element address
				LDR			R4,[R7]	                ;r4 = next element
                CMP         r2,R4                   ;compare next element with 0
                BEQ         rmvNotFound             ;if equal input not found
                B           rmvCntrl                ;if not continue with next element
rmvFound        LDR			R4,[R1]                 ;r4 = first element
				CMP        	R4,r7                   ;compare first element with current value
                BEQ         rmvFirst                ;if equal remove first element
                ADDS        r7,r7,#4                ;if not go next element
                LDR         r5,[r7]                 ;r5 = next element
                STR         r5,[r3]                 ;r3 = next element
                SUBS        r7,r7,#4                ;previous element
                MOV         r0,r7                   ;r0 = input element
                BL          Free                    ;jump to free to free input element address
                MOVS        r0,#0                   ;r0 = 0
                B           rmvFinish               ;jump to finish with success code
rmvFirst        mov         r5,r7                   ;r5 = first element of linked list
                ADDS        r5,r5,#4                ;r5 = second elements address of linked list
                LDR         r5,[r5]                 ;r5 = second element of linked list
                STR         r5,[r1]                 ;first element shows second element
                MOV         r0,r7                   ;r0 = first element of linked list
                BL          Free                    ;jump to free to free first elements address
				MOVS		r0,#0                   ;success code
				B			rmvFinish               ;jump finish
rmvEmpty        MOVS        r0,#3                   ;r0 = 3
                B           rmvFinish               ;return with error code 3
rmvNotFound     MOVS        r0,#4                   ;r0 = 4
                B           rmvFinish               ;return with error code 4
rmvFinish       POP 		{r1-r6}					;replacing old values of the registers
				POP			{r7}                    ;pop return address from stack
				BX 			R7						; return		
;//-------- <<< USER CODE END Remove Function >>> ------------------------				
				ENDFUNC
				
;*******************************************************************************				

;@brief 	This function will be used to clear the array and copy the linked list to the array
;@return	R0 <- Error Code
LinkedList2Arr	FUNCTION			
;//-------- <<< USER CODE BEGIN Linked List To Array >>> ----------------------
				PUSH	    {LR}				    ;push return address to stack
				PUSH	    {r1-r6}			        ;storing old values of registers
                LDR         r7,=ARRAY_MEM           ;r7 = ARRAY_MEM[0]
                MOVS        r0,#0                   ;index
                LDR       	r1,=ARRAY_SIZE          ;total size
lnkdChck        CMP         r1,r0                   ;check for remaining bits
                BEQ         lnkdEndClr              ;if there is not bit remaining jump end clear
                MOVS        r2,#0                   ;if there is remaining bits, r2 = 0
                STR         r2,[r7,r0]              ;clear selected index
                ADDS        r0,#4                   ;index++
                B           lnkdChck                ;check index
lnkdEndClr      LDR         r7,=FIRST_ELEMENT       ;r7 = first element address
				LDR			R3,[R7]                 ;r3 = first element address
                CMP         r2,R3                   ;compare 0 with first element address
                BEQ         lnkdEmpty               ;if equal linked list is empty
                LDR         r6,=ARRAY_MEM           ;r6 = array_mem address
                MOVS        r4,#0                   ;r4 = 0 (index)
				LDR         r5,[r7]                 ;r5 = linked_list[r4] address
lnkdLoop		LDR			r3,[r5]					;r3 = linked_list[r4] value
                STR         r3,[r6,r4]              ;array_mem[r4] = linked_list[r4]
                ADDS        r5,r5,#4                ;r7 = next element
				LDR			r5,[r5]					;load the next address
                ADDS        r4,r4,#4                ;index++
                MOVS        r0,#0                   ;r0 = 0 for return successly value
                CMP         r2,r5                   ;compare 0 with next element
                BEQ         lnkdFinish              ;if equal end of the linked list
                B           lnkdLoop                ;go to loop
lnkdEmpty       MOVS        r0,#5                   ;if linked list empty return with error code 5
                B           lnkdFinish              ;go to finish
				MOVS        r0,#0                   ;return with the success value
                B           lnkdFinish				;jump to lndkFinish
lnkdFinish      POP         {r1-r6}                 ;replacing old values of the registers
				pop			{r7}					;pop return address from stack
                BX          r7						;return
;//-------- <<< USER CODE END Remove Function >>> ------------------------				
				ENDFUNC

;@brief 	This function will be used to write errors to the error log array.
;@param		R0 -> Index of Input Dataset Array
;@param     R1 -> Error Code 
;@param     R2 -> Operation (Insertion / Deletion / LinkedList2Array)
;@param     R3 -> Data
WriteErrorLog	FUNCTION			
;//-------- <<< USER CODE BEGIN Write Error Log >>> ----------------------
				PUSH    {LR}							; push return address to stack
                PUSH	{r0-r6}                         ; storing old values of registers
                CMP		R1,#0                           ; if there is no error, nothing to write
                BEQ     endWEL                          ; end WriteErrorLog function
				LDR     R4,=LOG_MEM                     ; R4, address of error log array
                LDR    	R5,=INDEX_ERROR_LOG     		; address of index of error log array
                LDR     R6,[R5]                         ; R6, index value
				ADDS    R4,R4,R6                        ; current available adress of LOG_MEM
				LDR     R7,=__LOG_END           		; address of end of LOG_MEM
                CMP     R4,R7                           ; check if LOG_MEM is full
                BEQ     endWEL                          ; if its full end this function
                STRH    R0,[R4,#0]                      ; load index (16 bits)
                STRB    R1,[R4,#2]                      ; load error code (8 bits)
                STRB    R2,[R4,#3]                      ; load operation (8 bits)
				STR     R3,[R4,#4]                      ; load data
                BL       GetNow                         ; get current time
contlog         STR     R0,[R4,#8]                      ; load timestamp
                ADDS    R6,R6,#12                       ; increment index of error log array
                STR     R6,[R5]                         ; store new index in global variable INDEX_ERROR_LOG
endWEL   		POP     {r0-r6}                         ; replacing old values of the registers
				pop		{r7}							; pop return address from stack
                BX      r7                              ; return 	
;//-------- <<< USER CODE END Write Error Log >>> ------------------------				
				ENDFUNC
				
;@brief 	This function will be used to get working time of the System Tick timer
;@return	R0 <- Working time of the System Tick Timer (in us).			
GetNow			FUNCTION			
;//-------- <<< USER CODE BEGIN Get Now >>> ----------------------
				PUSH    {LR}									; push return address to stack
                PUSH    {r1-r6}                                 ; storing old values of registers
                LDR   	R1,=TICK_COUNT                 			; getting the address of tick_count
                LDR     R0,[R1]                                 ; getting the value of tick_count, r0
                LSRS    R0,#2                                   ; tick_count = tick_count / 4     (increment 4 each time)
                LDR     R3,=827                                 ; Period Of the System Tick Timer Interrupt: 827 ?s
				MULS    R0,R3,R0                                ; R0 = tick_count * period
                LDR     R3,=0xE000E018                  		; address of SysTick Current Value Register
                LDR     R2,[R3]                                 ; getting the value of current value register, r2
                LDR     R4,=6615                                ; reload value
                SUBS    R4,R4,R2                                ; current value = reload value - value
                ADDS    R4,R4,#1                                ; current= current+1
                LSRS    R4,#3                                   ; (Current value+1)/FCPU  , FPCU = 8 MHz
				ADDS    R0,R0,R4                                ; working time of the system tick timer in ?s
                POP     {r1-r6}                                 ; replacing old values of the registers
				POP    	{r7}									; pop return address from stack
                BX      r7                                		; return    		
;//-------- <<< USER CODE END Get Now >>> ------------------------
				ENDFUNC
				
	

;//-------- <<< USER CODE END Functions >>> ------------------------

;*******************************************************************************
;@endsection 		USER_FUNCTIONS
;*******************************************************************************
				ALIGN
				END		; Finish the assembly file
				
;*******************************************************************************
;@endfile 			main.s
;*******************************************************************************				

