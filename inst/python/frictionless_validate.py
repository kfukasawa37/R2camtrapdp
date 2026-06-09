#!/usr/bin/env python
"""Validate a Frictionless / Camtrap DP data package and emit a JSON report.

Usage:
    python frictionless_validate.py path/to/datapackage.json

The script prints a single JSON object to stdout:
  * normally: the Frictionless report as a dict (``report.to_dict()``),
  * if Frictionless raises before producing a report (e.g. invalid package
    metadata): a synthesised report dict ``{"valid": false, "errors": [...]}``,
  * on an environment failure (e.g. frictionless not installed):
    ``{"error": "<message>"}``.

It changes the working directory to the package directory and validates the
package by its file name, which (a) lets relative resource paths such as
``deployments.csv`` resolve and (b) avoids Frictionless mistaking a Windows
drive letter (``d:``) for a URL scheme.

It is invoked from R by ``R6_CamtrapDP$validate_frictionless()`` via
``system2()``; R parses the JSON into a uniform issue table.

Requires the ``frictionless`` package::

    pip install frictionless
"""
import os
import sys
import json


def _emit(obj):
    sys.stdout.write(json.dumps(obj, default=str))
    sys.stdout.flush()


def _err_to_dict(err):
    if hasattr(err, "to_dict"):
        try:
            return err.to_dict()
        except Exception:
            pass
    return {
        "type": getattr(err, "type", getattr(err, "code", "general-error")),
        "title": getattr(err, "title", "Error"),
        "note": getattr(err, "note", str(err)),
        "message": str(err),
    }


def _report_from_exception(exc):
    """Turn a FrictionlessException (raised before a report exists) into a
    report-shaped dict so the R side can report it uniformly."""
    errors = []
    err = getattr(exc, "error", None)
    if err is not None:
        errors.append(_err_to_dict(err))
    for reason in (getattr(exc, "reasons", None) or []):
        errors.append(_err_to_dict(reason))
    if not errors:
        errors.append({"type": "general-error", "title": "Error", "note": str(exc),
                       "message": str(exc)})
    return {"valid": False, "errors": errors, "tasks": []}


def main(argv):
    if len(argv) < 2:
        _emit({"error": "Usage: frictionless_validate.py <datapackage.json>"})
        return 2

    path = argv[1]

    try:
        import frictionless  # noqa: F401
    except Exception as exc:  # pragma: no cover - environment dependent
        _emit({"error": "frictionless is not installed: %s. "
                        "Install it with 'pip install frictionless'." % exc})
        return 1

    from frictionless import Package, FrictionlessException

    directory = os.path.dirname(os.path.abspath(path)) or "."
    name = os.path.basename(path)
    try:
        os.chdir(directory)
    except Exception as exc:
        _emit({"error": "Cannot enter package directory %r: %s" % (directory, exc)})
        return 1

    try:
        package = Package(name)
        report = package.validate()
        report_dict = report.to_dict() if hasattr(report, "to_dict") else dict(report)
        _emit(report_dict)
        return 0
    except FrictionlessException as exc:
        # Invalid metadata etc. -> surface as a report instead of crashing.
        _emit(_report_from_exception(exc))
        return 0
    except Exception as exc:  # pragma: no cover - environment dependent
        _emit({"error": "Validation failed: %s" % exc})
        return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
