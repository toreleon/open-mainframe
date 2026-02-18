"use client";

import { useState } from "react";
import type { AssessmentReport } from "@/lib/types";

interface AssessmentCardProps {
  report: AssessmentReport | null;
  onViewFullReport?: () => void;
}

const SEVERITY_COLORS: Record<string, string> = {
  critical: "text-om-error",
  high: "text-om-warning",
  warning: "text-om-warning",
  info: "text-om-info",
};

const SEVERITY_DOTS: Record<string, string> = {
  critical: "bg-om-error",
  high: "bg-om-warning",
  warning: "bg-om-warning",
  info: "bg-om-info",
};

export function AssessmentCard({ report, onViewFullReport }: AssessmentCardProps) {
  const [expanded, setExpanded] = useState(false);

  if (!report) {
    return (
      <div className="bg-om-surface border border-om-border rounded-lg p-3 text-xs text-om-muted">
        No assessment data available.
      </div>
    );
  }

  const issueCount = report.issues.length;
  const criticalCount = report.issues.filter((i) => i.severity === "critical").length;
  const totalDebt = report.programs.reduce((sum, p) => sum + p.technical_debt_hours, 0);

  return (
    <div className="bg-om-surface border border-om-border rounded-lg overflow-hidden max-w-lg">
      {/* Summary header — always visible */}
      <div className="p-4">
        <div className="flex items-center justify-between mb-3">
          <div className="text-xs font-semibold text-om-accent">Assessment Complete</div>
          <div className="flex items-center gap-2">
            {onViewFullReport && (
              <button
                onClick={onViewFullReport}
                className="text-[10px] text-om-accent hover:text-om-text transition-colors"
              >
                Full Report
              </button>
            )}
            <button
              onClick={() => setExpanded(!expanded)}
              className="text-[10px] text-om-muted hover:text-om-accent transition-colors"
            >
              {expanded ? "Collapse" : "View Details"}
            </button>
          </div>
        </div>
        <div className="grid grid-cols-3 gap-3 text-center">
          <div>
            <div className="text-lg font-bold text-om-text">{report.total_files}</div>
            <div className="text-[10px] text-om-muted">Files</div>
          </div>
          <div>
            <div className="text-lg font-bold text-om-text">{report.total_loc.toLocaleString()}</div>
            <div className="text-[10px] text-om-muted">LOC</div>
          </div>
          <div>
            <div className="text-lg font-bold text-om-text">{report.average_complexity.toFixed(1)}</div>
            <div className="text-[10px] text-om-muted">Avg Complexity</div>
          </div>
        </div>
        {issueCount > 0 && (
          <div className="text-xs text-om-muted mt-3">
            {issueCount} issue{issueCount !== 1 ? "s" : ""} found
            {criticalCount > 0 && (
              <span className="text-om-error ml-1">({criticalCount} critical)</span>
            )}
            {totalDebt > 0 && (
              <span className="ml-2">| ~{Math.round(totalDebt)}h tech debt</span>
            )}
          </div>
        )}
      </div>

      {/* Expanded detail — inline dashboard */}
      {expanded && (
        <div className="border-t border-om-border p-4 space-y-4">
          {/* Feature support */}
          {Object.keys(report.feature_support).length > 0 && (
            <div>
              <h4 className="text-xs font-semibold text-om-text mb-2">Feature Support</h4>
              <div className="space-y-1.5">
                {Object.entries(report.feature_support)
                  .sort(([, a], [, b]) => b - a)
                  .map(([feature, pct]) => {
                    const color = pct >= 80 ? "bg-om-success" : pct >= 50 ? "bg-om-warning" : "bg-om-error";
                    return (
                      <div key={feature} className="flex items-center gap-2 text-[10px]">
                        <span className="text-om-muted w-20 truncate text-right">{feature}</span>
                        <div className="flex-1 h-1.5 bg-om-border/50 rounded-full overflow-hidden">
                          <div className={`h-full ${color} rounded-full`} style={{ width: `${pct}%` }} />
                        </div>
                        <span className="text-om-muted w-8 text-right">{pct}%</span>
                      </div>
                    );
                  })}
              </div>
            </div>
          )}

          {/* Issues */}
          {issueCount > 0 && (
            <div>
              <h4 className="text-xs font-semibold text-om-text mb-2">Issues ({issueCount})</h4>
              <div className="space-y-1 max-h-48 overflow-y-auto">
                {report.issues.map((issue, idx) => (
                  <div
                    key={idx}
                    className="flex items-center gap-2 px-2 py-1.5 text-[10px] rounded bg-om-bg/50"
                  >
                    <span className={`w-1.5 h-1.5 rounded-full shrink-0 ${SEVERITY_DOTS[issue.severity] || "bg-om-muted"}`} />
                    <span className={`uppercase font-bold w-12 shrink-0 ${SEVERITY_COLORS[issue.severity] || "text-om-muted"}`}>
                      {issue.severity}
                    </span>
                    <span className="text-om-text font-mono truncate">
                      {issue.file_path.split("/").pop()}
                      {issue.line != null && `:${issue.line}`}
                    </span>
                    <span className="text-om-muted truncate ml-auto">{issue.message}</span>
                  </div>
                ))}
              </div>
            </div>
          )}

          {/* Programs */}
          {report.programs.length > 0 && (
            <div>
              <h4 className="text-xs font-semibold text-om-text mb-2">Programs ({report.programs.length})</h4>
              <div className="overflow-x-auto">
                <table className="w-full text-[10px]">
                  <thead>
                    <tr className="border-b border-om-border text-om-muted text-left">
                      <th className="pb-1.5 pr-3">Program</th>
                      <th className="pb-1.5 pr-3">LOC</th>
                      <th className="pb-1.5 pr-3">Complexity</th>
                      <th className="pb-1.5">Maintainability</th>
                    </tr>
                  </thead>
                  <tbody>
                    {report.programs
                      .sort((a, b) => b.complexity - a.complexity)
                      .map((prog) => (
                        <tr key={prog.program_id} className="border-b border-om-border/30">
                          <td className="py-1.5 pr-3 font-mono text-om-accent">{prog.program_id}</td>
                          <td className="py-1.5 pr-3 text-om-text">{prog.loc.toLocaleString()}</td>
                          <td className="py-1.5 pr-3 text-om-text">{prog.complexity.toFixed(1)}</td>
                          <td className="py-1.5 text-om-text">{prog.maintainability}/100</td>
                        </tr>
                      ))}
                  </tbody>
                </table>
              </div>
            </div>
          )}

          {/* Recommendations */}
          {report.recommendations.length > 0 && (
            <div>
              <h4 className="text-xs font-semibold text-om-text mb-2">Recommendations</h4>
              <ul className="space-y-1.5">
                {report.recommendations.map((rec, idx) => (
                  <li key={idx} className="flex gap-2 text-[10px] text-om-muted">
                    <span className="text-om-accent shrink-0">-</span>
                    <span>{rec}</span>
                  </li>
                ))}
              </ul>
            </div>
          )}
        </div>
      )}
    </div>
  );
}
