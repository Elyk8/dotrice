def latex_template(name, title):
    return '\n'.join((r"\begin{figure}[H]",
                      r"    \centering",
                      rf"    \incfig[0.8]{{{name}}}",
                      rf"    \caption{{{title}}}",
                      rf"    \label{{fig:{name}}}",
                      r"    \vspace{-0.5cm}",
                      r"\end{figure}"))
