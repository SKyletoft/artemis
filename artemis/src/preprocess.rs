fn remove_comment_from_line(line: &str) -> &str {
	match line.find("//") {
		Some(idx) => &line[..idx],
		None => line,
	}
}

fn remove_multiline_comment(file: &str) -> (&str, &str) {
	if let Some(start) = file.find("/*") {
		if let Some(end) = file[start..].find("*/") {
			(&file[..start], &file[(start + end + 2)..])
		} else {
			(&file[..start], "")
		}
	} else {
		(file, "")
	}
}

pub fn remove_comments(files: &[String]) -> String {
	let mut out = String::new();

	for file in files {
		let mut rest = file.as_str();
		while !rest.is_empty() {
			let first;
			(first, rest) = remove_multiline_comment(rest);

			for line in first.lines().map(remove_comment_from_line) {
				out.push_str(line);
				out.push('\n');
			}
		}
	}

	out
}
