// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Primary design header
//
// This header should be included by all source files instantiating the design.
// The class here is then constructed to instantiate the design.
// See the Verilator manual for examples.

#ifndef _VAdder_H_
#define _VAdder_H_

#include "verilated.h"
class VAdder__Syms;

//----------

VL_MODULE(VAdder) {
  public:
    // CELLS
    // Public to allow access to /*verilator_public*/ items;
    // otherwise the application code can consider these internals.
    
    // PORTS
    // The application code writes and reads these signals to
    // propagate new values into/out from the Verilated model.
    
    // LOCAL SIGNALS
    // Internals; generally not touched by application code
    
    // LOCAL VARIABLES
    // Internals; generally not touched by application code
    
    // INTERNAL VARIABLES
    // Internals; generally not touched by application code
    //char	__VpadToAlign12[4];
    VAdder__Syms*	__VlSymsp;		// Symbol table
    
    // PARAMETERS
    // Parameters marked /*verilator public*/ for use by application code
    
    // CONSTRUCTORS
  private:
    VAdder& operator= (const VAdder&);	///< Copying not allowed
    VAdder(const VAdder&);	///< Copying not allowed
  public:
    /// Construct the model; called by application code
    /// The special name  may be used to make a wrapper with a
    /// single model invisible WRT DPI scope names.
    VAdder(const char* name="TOP");
    /// Destroy the model; called (often implicitly) by application code
    ~VAdder();
    
    // USER METHODS
    
    // API METHODS
    /// Evaluate the model.  Application must call when inputs change.
    void eval();
    /// Simulation complete, run final blocks.  Application must call on completion.
    void final();
    
    // INTERNAL METHODS
  private:
    static void _eval_initial_loop(VAdder__Syms* __restrict vlSymsp);
  public:
    void __Vconfigure(VAdder__Syms* symsp, bool first);
  private:
    static QData	_change_request(VAdder__Syms* __restrict vlSymsp);
  public:
    static void	_eval(VAdder__Syms* __restrict vlSymsp);
    static void	_eval_initial(VAdder__Syms* __restrict vlSymsp);
    static void	_eval_settle(VAdder__Syms* __restrict vlSymsp);
} VL_ATTR_ALIGNED(128);

#endif  /*guard*/
