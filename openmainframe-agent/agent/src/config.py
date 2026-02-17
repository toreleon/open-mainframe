"""
Environment configuration and model factory.
"""

import os

from langchain_openai import ChatOpenAI


def get_model(config=None):
    """Create an LLM instance based on environment configuration.

    Supports:
      LLM_PROVIDER=openai (default) | anthropic
      LLM_MODEL=gpt-4.1-mini (default) | claude-sonnet-4-5-20250929 | etc.
    """
    provider = os.getenv("LLM_PROVIDER", "openai")
    model_name = os.getenv("LLM_MODEL", "gpt-4.1-mini")

    if provider == "anthropic":
        from langchain_anthropic import ChatAnthropic

        return ChatAnthropic(model=model_name)

    return ChatOpenAI(model=model_name)
