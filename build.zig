const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    const lib = b.addStaticLibrary("rezolv", "src/rezolv.zig");
    lib.setBuildMode(mode);
    lib.install();

    var main_tests = b.addTest("src/rezolv.zig");
    main_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
}
