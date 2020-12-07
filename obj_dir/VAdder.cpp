// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See VAdder.h for the primary calling header

#include "VAdder.h"            // For This
#include "VAdder__Syms.h"

//--------------------
// STATIC VARIABLES


//--------------------

VL_CTOR_IMP(VAdder) {
    VAdder__Syms* __restrict vlSymsp = __VlSymsp = new VAdder__Syms(this, name());
    VAdder* __restrict vlTOPp VL_ATTR_UNUSED = vlSymsp->TOPp;
    // Reset internal values
    
    // Reset structure values
}

void VAdder::__Vconfigure(VAdder__Syms* vlSymsp, bool first) {
    if (0 && first) {}  // Prevent unused
    this->__VlSymsp = vlSymsp;
}

VAdder::~VAdder() {
    delete __VlSymsp; __VlSymsp=NULL;
}

//--------------------


void VAdder::eval() {
    VAdder__Syms* __restrict vlSymsp = this->__VlSymsp; // Setup global symbol table
    VAdder* __restrict vlTOPp VL_ATTR_UNUSED = vlSymsp->TOPp;
    // Initialize
    if (VL_UNLIKELY(!vlSymsp->__Vm_didInit)) _eval_initial_loop(vlSymsp);
    // Evaluate till stable
    VL_DEBUG_IF(VL_PRINTF("\n----TOP Evaluate VAdder::eval\n"); );
    int __VclockLoop = 0;
    QData __Vchange=1;
    while (VL_LIKELY(__Vchange)) {
	VL_DEBUG_IF(VL_PRINTF(" Clock loop\n"););
	vlSymsp->__Vm_activity = true;
	_eval(vlSymsp);
	__Vchange = _change_request(vlSymsp);
	if (++__VclockLoop > 100) vl_fatal(__FILE__,__LINE__,__FILE__,"Verilated model didn't converge");
    }
}

void VAdder::_eval_initial_loop(VAdder__Syms* __restrict vlSymsp) {
    vlSymsp->__Vm_didInit = true;
    _eval_initial(vlSymsp);
    vlSymsp->__Vm_activity = true;
    int __VclockLoop = 0;
    QData __Vchange=1;
    while (VL_LIKELY(__Vchange)) {
	_eval_settle(vlSymsp);
	_eval(vlSymsp);
	__Vchange = _change_request(vlSymsp);
	if (++__VclockLoop > 100) vl_fatal(__FILE__,__LINE__,__FILE__,"Verilated model didn't DC converge");
    }
}

//--------------------
// Internal Methods

void VAdder::_eval(VAdder__Syms* __restrict vlSymsp) {
    VL_DEBUG_IF(VL_PRINTF("    VAdder::_eval\n"); );
    VAdder* __restrict vlTOPp VL_ATTR_UNUSED = vlSymsp->TOPp;
}

void VAdder::_eval_initial(VAdder__Syms* __restrict vlSymsp) {
    VL_DEBUG_IF(VL_PRINTF("    VAdder::_eval_initial\n"); );
    VAdder* __restrict vlTOPp VL_ATTR_UNUSED = vlSymsp->TOPp;
}

void VAdder::final() {
    VL_DEBUG_IF(VL_PRINTF("    VAdder::final\n"); );
    // Variables
    VAdder__Syms* __restrict vlSymsp = this->__VlSymsp;
    VAdder* __restrict vlTOPp VL_ATTR_UNUSED = vlSymsp->TOPp;
}

void VAdder::_eval_settle(VAdder__Syms* __restrict vlSymsp) {
    VL_DEBUG_IF(VL_PRINTF("    VAdder::_eval_settle\n"); );
    VAdder* __restrict vlTOPp VL_ATTR_UNUSED = vlSymsp->TOPp;
}

VL_INLINE_OPT QData VAdder::_change_request(VAdder__Syms* __restrict vlSymsp) {
    VL_DEBUG_IF(VL_PRINTF("    VAdder::_change_request\n"); );
    VAdder* __restrict vlTOPp VL_ATTR_UNUSED = vlSymsp->TOPp;
    // Body
    // Change detection
    QData __req = false;  // Logically a bool
    return __req;
}
