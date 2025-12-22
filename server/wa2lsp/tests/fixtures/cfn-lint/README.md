# cfn-lint test fixtures
* Vendored from: https://github.com/aws-cloudformation/cfn-lint
* Commit: [54a7030](https://github.com/aws-cloudformation/cfn-lint/commit/54a70306b424e76058bfc9fe0d7dd3612dec7c3c)
* Date: 2025-12-22

To update, re-run the sparse checkout and copy. code below assume linus with a /tmp folder

```bash
# from "tests" directory (where this file is!)
git clone --depth 1 --filter=blob:none --sparse \
  https://github.com/aws-cloudformation/cfn-lint.git /tmp/cfn-lint
pushd /tmp/cfn-lint
git sparse-checkout set test/fixtures/templates
popd
cp -r /tmp/cfn-lint/test/fixtures/templates .

# remove any python as we don't use
find . -name "*.py" -delete
````