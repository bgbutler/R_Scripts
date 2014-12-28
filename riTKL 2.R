filename <- tclvalue(tkgetOpenFile())
  if (!nchar(filename)) {
    tkmessageBox(message = "No file was selected!")}
    else {
      tkmessageBox(message = paste("The file selected was", filename))
    }
  }


program <- tktoplevel()
heading <- tklabel(program, text = "Reinsurance Program")
l.limit <- tklabel(program, text = "Limit")
l.cession <- tklabel(program, text = "Cession")
l.attach <- tklabel(program, text = "Attachment")
e.limit <- tkentry(program, width = 10)
e.cession <- tkentry(program, width = 10)
e.attach <- tkentry(program, width = 10)

tkgrid(heading, columspan=2)
tkgrid(l.limit, e.limit)
tkgrid(l.cession, e.cession)
tkgrid(l.attach, e.attach)
tkgrid.configure(l.limit, l.cession, l.attach, sticky="e")
tkgrid.configure(e.limit, e.cession, e.attach, sticky="w")

