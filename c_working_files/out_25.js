const f = (r0) => {
	let from = 0;
	let at = 0;
	const regs = {
		r0: r0
	};
	
	while (true) {
		switch (at) {
			case 0:
				regs.r1 = 5 + 2;
				regs.r2 = 3 + 3;

				from = at;
				if (regs.r0 !== 0) {
					at = 1;
				} else {
					at = 2;
				}
				break;
			case 1:
				regs.r3 = 1 * regs.r2;
				regs.r4 = 2 + regs.r3;
				regs.r5 = regs.r4 - 43;
				regs.r6 = 1 + 2;

				from = at;
				at = 3;
				break;
			case 2:
				regs.r7 = 3 + 1;
				regs.r8 = 2 + 3;

				from = at;
				at = 3;
				break;
			case 3:
				if (from == 1) {
					regs.r9 = regs.r5;
					regs.r10 = regs.r6;
				} else if (from == 2) {
					regs.r9 = regs.r7;
					regs.r10 = regs.r8;
				}
				
				regs.r11 = regs.r9 + regs.r10;
				regs.r12 = regs.r11 + regs.r1;

				return regs.r12;
		}
	}
};

console.log(f(1) + "\n" + f(0));
