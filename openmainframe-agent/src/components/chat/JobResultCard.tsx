"use client";

import { useState } from "react";
import type { ExecutionResult } from "@/lib/types";

interface JobResultCardProps {
  result: ExecutionResult | null;
  jclFile?: string;
}

function rcColor(rc: number): string {
  if (rc === 0) return "text-om-success";
  if (rc <= 4) return "text-om-warning";
  return "text-om-error";
}

function rcDot(rc: number): string {
  if (rc === 0) return "bg-om-success";
  if (rc <= 4) return "bg-om-warning";
  return "bg-om-error";
}

export function JobResultCard({ result, jclFile }: JobResultCardProps) {
  const [expanded, setExpanded] = useState(false);

  if (!result) {
    return (
      <div className="bg-om-surface border border-om-border rounded-lg p-4 max-w-lg">
        <div className="flex items-center gap-2">
          <span className="text-om-success text-sm">&#10003;</span>
          <span className="text-xs text-om-text">
            Job complete{jclFile ? `: ${jclFile.split("/").pop()}` : ""}
          </span>
        </div>
      </div>
    );
  }

  const jobName = result.jcl_file.split("/").pop() || result.jcl_file;
  const totalDuration = result.steps.reduce((sum, s) => sum + s.duration_ms, 0);
  const maxRc = result.max_return_code;
  const success = maxRc === 0;

  return (
    <div className="bg-om-surface border border-om-border rounded-lg overflow-hidden max-w-lg">
      {/* Summary */}
      <div className="p-4">
        <div className="flex items-center justify-between mb-2">
          <span className={`text-xs font-semibold ${success ? "text-om-success" : "text-om-error"}`}>
            {success ? "Job Completed Successfully" : "Job Completed with Errors"}
          </span>
          {result.steps.length > 0 && (
            <button
              onClick={() => setExpanded(!expanded)}
              className="text-[10px] text-om-muted hover:text-om-accent transition-colors"
            >
              {expanded ? "Collapse" : "View Steps"}
            </button>
          )}
        </div>
        <div className="text-xs text-om-muted">
          Job: <span className="font-mono text-om-text">{jobName}</span>
        </div>
        <div className="flex gap-3 mt-1 text-xs">
          <span className="text-om-muted">{result.steps.length} steps</span>
          <span className={rcColor(maxRc)}>Max RC={maxRc}</span>
          <span className="text-om-muted">{(totalDuration / 1000).toFixed(1)}s</span>
        </div>
      </div>

      {/* Expanded step timeline */}
      {expanded && result.steps.length > 0 && (
        <div className="border-t border-om-border p-4 space-y-2 max-h-60 overflow-y-auto">
          {result.steps.map((step, idx) => (
            <div key={idx} className="flex items-center gap-3 text-[10px]">
              <span className={`w-2 h-2 rounded-full shrink-0 ${rcDot(step.return_code)}`} />
              <span className="font-mono text-om-text w-16 shrink-0">{step.step_name}</span>
              <span className="text-om-muted w-16 shrink-0">{step.program}</span>
              <span className={`${rcColor(step.return_code)} w-10 shrink-0`}>RC={step.return_code}</span>
              <span className="text-om-muted">{(step.duration_ms / 1000).toFixed(1)}s</span>
              {step.output && (
                <span className="text-om-muted truncate ml-auto max-w-[200px]" title={step.output}>
                  {step.output.split("\n")[0]}
                </span>
              )}
            </div>
          ))}
        </div>
      )}
    </div>
  );
}
