using DocStringExtensions

@template MODULES = """
    $(EXPORTS)
    $(DOCSTRING)
    """

@template TYPES = """
    $(TYPEDEF)
    $(DOCSTRING)
    $(TYPEDFIELDS)
    """

@template FUNCTIONS = """
    $(DOCSTRING)
    $(METHODLIST)
    """
@template (METHODS, MACROS) = """
    $(TYPEDSIGNATURES)
    $(DOCSTRING)
    """