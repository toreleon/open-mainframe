"use client";

import { useEffect, useCallback, type ReactNode } from "react";

interface DrawerProps {
  open: boolean;
  onClose: () => void;
  title: string;
  children: ReactNode;
}

export function Drawer({ open, onClose, title, children }: DrawerProps) {
  const handleKeyDown = useCallback(
    (e: KeyboardEvent) => {
      if (e.key === "Escape") onClose();
    },
    [onClose],
  );

  useEffect(() => {
    if (open) {
      document.addEventListener("keydown", handleKeyDown);
      return () => document.removeEventListener("keydown", handleKeyDown);
    }
  }, [open, handleKeyDown]);

  if (!open) return null;

  return (
    <div className="fixed inset-0 z-50 flex justify-end">
      {/* Backdrop */}
      <div
        className="absolute inset-0 bg-black/40 transition-opacity"
        onClick={onClose}
      />

      {/* Drawer panel */}
      <div className="relative w-[60%] max-w-3xl bg-om-surface border-l border-om-border shadow-2xl flex flex-col animate-slide-in">
        {/* Header */}
        <div className="flex items-center justify-between px-6 py-4 border-b border-om-border shrink-0">
          <h2 className="text-sm font-semibold text-om-text">{title}</h2>
          <button
            onClick={onClose}
            className="text-om-muted hover:text-om-text transition-colors text-lg leading-none"
          >
            &times;
          </button>
        </div>

        {/* Content */}
        <div className="flex-1 overflow-auto">{children}</div>
      </div>
    </div>
  );
}
