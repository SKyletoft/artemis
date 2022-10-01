fn remove_comment_from_line(line: &str) -> &str {
	match line.find("//") {
		Some(idx) => &line[..idx],
		None => line,
	}
}

pub fn remove_comments(files: &[String]) -> String {
	files.iter()
		.flat_map(|file| file.lines())
		.map(remove_comment_from_line)
		.fold(String::new(), |mut acc, curr| {
			acc.push_str(curr);
			acc.push('\n');
			acc
		})
}
