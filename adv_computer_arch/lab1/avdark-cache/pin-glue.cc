/**
 * Glue code for running the AvDark cache simulator in Pin.
 *
 * Course: Advanced Computer Architecture, Uppsala University
 * Course Part: Lab assignment 1
 *
 * Author: Andreas Sandberg <andreas.sandberg@it.uu.se>
 *
 * $Id: pin-glue.cc 85 2012-09-13 20:33:07Z andse541 $
 */


#include "pin.H"

#include <iostream>
#include <fstream>

#include <sys/time.h>

extern "C" {
#include "avdark-cache.h"
}

KNOB<string> knob_output(KNOB_MODE_WRITEONCE,    "pintool",
                         "o", "avdc.out", "specify log file name");

KNOB<UINT32> knob_size(KNOB_MODE_WRITEONCE, "pintool",
		       "s", "8388608", "Cache size (bytes)");
KNOB<UINT32> knob_associativity(KNOB_MODE_WRITEONCE, "pintool",
				"a", "1", "Cache associativity");
KNOB<UINT32> knob_line_size(KNOB_MODE_WRITEONCE, "pintool",
			    "l", "64", "Cache line size");

static avdark_cache_t *avdc = NULL;

/**
 * Memory access callback. Will be called for every memory access
 * executed by the the target application.
 */
static VOID
simulate_access(VOID *addr, UINT32 access_type)
{

        /* NOTE: We deliberately ignore the fact that addr may
         * straddle multiple cache lines */
        avdc_access(avdc, (avdc_pa_t)addr, (avdc_access_type_t)access_type);

}

/**
 * PIN instrumentation callback, called for every new instruction that
 * PIN discovers in the application. This function is used to
 * instrument code blocks by inserting calls to instrumentation
 * functions.
 */
static VOID
instruction(INS ins, VOID *not_used)
{
        UINT32 no_ops = INS_MemoryOperandCount(ins);

        for (UINT32 op = 0; op < no_ops; op++) {
                const UINT32 size = INS_MemoryOperandSize(ins, op);
                const bool is_rd = INS_MemoryOperandIsRead(ins, op);
                const bool is_wr = INS_MemoryOperandIsWritten(ins, op);
                const UINT32 atype = is_wr ? AVDC_WRITE : AVDC_READ;
                // printf("calling the instruction function; params: %d,%d,%d,%d\n",size,is_rd,is_wr,atype );
                INS_InsertCall(ins, IPOINT_BEFORE,
                               (AFUNPTR)simulate_access,
                               IARG_MEMORYOP_EA, op,
                               IARG_UINT32, atype,
                               IARG_END); 
        }
}

/**
 * PIN fini callback. Called after the target application has
 * terminated. Used to print statistics and do cleanup.
 */
static VOID
fini(INT32 code, VOID *v)
{
        std::ofstream out(knob_output.Value().c_str());
        int accesses = avdc->stat_data_read + avdc->stat_data_write;
        int misses = avdc->stat_data_read_miss + avdc->stat_data_write_miss;

        out << "Cache statistics:" << endl;
        out << "  Writes: " << avdc->stat_data_write << endl;
        out << "  Write Misses: " << avdc->stat_data_write_miss << endl;
        out << "  Reads: " << avdc->stat_data_read << endl;
        out << "  Read Misses: " << avdc->stat_data_read_miss << endl;
        out << "  Misses: " << misses << endl;
        out << "  Accesses: " << accesses << endl;
        out << "  Miss Ratio: " << ((100.0 * misses) / accesses) << "%" << endl;

        avdc_delete(avdc);
}

static int
usage()
{
        cerr <<
                "This is the Advanced Computer Architecture online analysis tool\n"
                "\n";

        cerr << KNOB_BASE::StringKnobSummary();

        cerr << endl;

        return -1;
}

int main(int argc, char *argv[])
{
        if (PIN_Init(argc, argv))
                return usage();

        avdc_size_t size = knob_size.Value();
        avdc_block_size_t block_size = knob_line_size.Value();
        avdc_assoc_t assoc = knob_associativity.Value();

        avdc = avdc_new(size, block_size, assoc);
        if (!avdc) {
                cerr << "Failed to initialize the AvDark cache simulator." << endl;
                return -1;
        }

        INS_AddInstrumentFunction(instruction, 0);
        PIN_AddFiniFunction(fini, 0);

        PIN_StartProgram();
        return 0;
}

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 8
 * indent-tabs-mode: nil
 * c-file-style: "linux"
 * compile-command: "make -k -C ../../"
 * End:
 */
