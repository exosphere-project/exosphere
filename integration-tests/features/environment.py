import os

from behaving import environment as benv

PERSONAS = {
}

BEHAVE_DEBUG_ON_ERROR = False


def setup_debug_on_error(userdata):
    global BEHAVE_DEBUG_ON_ERROR
    BEHAVE_DEBUG_ON_ERROR = userdata.getbool("BEHAVE_DEBUG_ON_ERROR")


def before_all(context):
    setup_debug_on_error(context.config.userdata)
    context.remote_webdriver = context.config.userdata.getbool("REMOTE_WEBDRIVER", False)
    if not hasattr(context, "browser_args"):
        context.browser_args = {}
    command_executor = context.config.userdata.get("COMMAND_EXECUTOR")
    if command_executor:
        context.browser_args['command_executor'] = command_executor
    browser_brand = context.config.userdata.get("BROWSER", "Firefox")
    context.default_browser = browser_brand.lower()
    default_screenshots_dir = os.path.join(os.getcwd(), 'screenshots')
    print(default_screenshots_dir)
    context.screenshots_dir = context.config.userdata.get("SCREENSHOTS_DIR", default_screenshots_dir)
    benv.before_all(context)


def after_all(context):
    benv.after_all(context)


def before_feature(context, feature):
    benv.before_feature(context, feature)


def after_feature(context, feature):
    benv.after_feature(context, feature)


def before_scenario(context, scenario):
    benv.before_scenario(context, scenario)
    context.personas = PERSONAS


def after_scenario(context, scenario):
    benv.after_scenario(context, scenario)


def after_step(context, step):
    if BEHAVE_DEBUG_ON_ERROR and step.status == "failed":
        # -- ENTER DEBUGGER: Zoom in on failure location.
        # NOTE: Use PyCharm debugger. Same for IPython debugger and pdb (basic python debugger).
        import pydevd_pycharm
        pydevd_pycharm.settrace('localhost', port=4567, stdoutToServer=True,
                                stderrToServer=True, suspend=True)
