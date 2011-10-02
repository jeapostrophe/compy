	section .text
	        ;; make the main function externally visible
	global start                
	start:
        mov eax, 24
        ;; exit status returned to the operating system
	    push eax
        ;; system call number for exit
	    mov eax, 0x1
        ;; OS X (and BSD) system calls needs "extra space" on stack
	    sub esp, 4
        ;; make the system call
	    int 0x80 
