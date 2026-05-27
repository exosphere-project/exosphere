import os
import re

from behave import then, when, step
from behaving.personas.persona import persona_vars
from behaving.web.steps import i_press_xpath
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.ui import WebDriverWait


DEFAULT_LOCALIZATION = {
    "openstackWithOwnKeystone": "cloud",
    "openstackSharingKeystoneWithAnother": "region",
    "unitOfTenancy": "project",
    "maxResourcesPerProject": "resource limit",
    "pkiPublicKeyForSsh": "SSH public key",
    "virtualComputer": "instance",
    "virtualComputerHardwareConfig": "size",
    "cloudInitData": "boot script",
    "commandDrivenTextInterface": "terminal",
    "staticRepresentationOfBlockDeviceContents": "image",
    "blockDevice": "volume",
    "share": "share",
    "accessRule": "access rule",
    "exportLocation": "export location",
    "nonFloatingIpAddress": "internal IP address",
    "floatingIpAddress": "floating IP address",
    "publiclyRoutableIpAddress": "public IP address",
    "securityGroup": "firewall ruleset",
    "graphicalDesktopEnvironment": "graphical desktop",
    "hostname": "hostname",
    "credential": "credential",
}

DEFAULT_CONFIG = {
    "appTitle": "Exosphere",
    "localization": DEFAULT_LOCALIZATION,
}


@step('I pause for a breakpoint')
def i_pause_for_breakpoint(context):
    print('Stopping for a breakpoint')
    import pydevd_pycharm
    pydevd_pycharm.settrace('localhost', port=4567, stdoutToServer=True,
                            stderrToServer=True, suspend=True)


@step('I enable breakpoints')
def i_enable_breakpoints(context):
    print('Enabling breakpoints')
    import pydevd_pycharm
    pydevd_pycharm.settrace('localhost', port=4567, stdoutToServer=True,
                            stderrToServer=True, suspend=False)


@when('I go to Exosphere')
def i_go_to_exosphere(context):
    exosphere_url = context.config.userdata.get('EXOSPHERE_BASE_URL',
                                                'https://try.exosphere.app/exosphere')
    context.browser.visit(exosphere_url)


def find_by_label(context, label, element_type, wait_time=None):
    return context.browser.find_by_xpath(
        xpath=f"//label[contains(string(),'{label}')]//{element_type}",
        wait_time=wait_time
    )


def find_input_by_label(context, label, wait_time=None):
    return find_by_label(
        context=context,
        label=label,
        element_type='input',
        wait_time=wait_time)


@step(u'I fill input labeled "{label}" with "{value}"')
@persona_vars
def i_fill_input_labeled(context, label, value):
    element = find_input_by_label(context, label).first
    element.fill(value)


def find_element_with_role_and_label(context, role, label, wait_time=None):
    return context.browser.find_by_xpath(
        xpath=f"//div[@role='{role}']//div[contains(string(), '{label}')]",
        wait_time=wait_time
    )


def find_button_with_label(context, label, wait_time=None):
    return find_element_with_role_and_label(
        context=context,
        role='button',
        label=label,
        wait_time=wait_time
    )


def find_checkbox_with_label(context, label, wait_time=None):
    return context.browser.find_by_xpath(
        xpath=f"//label[@role='checkbox' and contains(string(), '{label}')]",
        wait_time=wait_time)


def find_radio_with_label(context, label, wait_time=None):
    return context.browser.find_by_xpath(
        xpath=f"//div[@role='radio' and contains(string(), '{label}')]",
        wait_time=wait_time)


def runtime_config(context):
    return context.browser.driver.execute_script(
        """
        var runtimeConfig = window.config || {};
        return {
          ...arguments[0],
          ...runtimeConfig,
          appTitle: runtimeConfig.appTitle ?? arguments[0].appTitle,
          defaultLoginView:
            runtimeConfig.defaultLoginView ?? arguments[0].defaultLoginView,
          localization: {
            ...arguments[0].localization,
            ...(runtimeConfig.localization || {})
          }
        };
        """,
        DEFAULT_CONFIG,
    )


def runtime_template_values(context):
    config = runtime_config(context)
    values = {
        key: value for key, value in config.items() if isinstance(value, str)
    }
    values.update(config["localization"])
    return values


def render_runtime_template(context, template, regex_escape_values=False):
    values = runtime_template_values(context)

    def replace_placeholder(match):
        key = match.group(1)
        assert key in values, (
            f'Unknown runtime template key "{key}" in template "{template}"'
        )
        value = values[key]
        return re.escape(value) if regex_escape_values else value

    return re.sub(r"{([A-Za-z0-9_]+)}", replace_placeholder, template)


def find_elements_by_xpath_matching_regex(context, xpath, pattern, timeout):
    compiled_pattern = re.compile(pattern, re.IGNORECASE)

    def matching_elements():
        return [
            element for element in context.browser.find_by_xpath(xpath)
            if compiled_pattern.search(element.text)
        ]

    if timeout <= 0:
        return matching_elements()

    try:
        return WebDriverWait(
            context.browser.driver,
            timeout,
            poll_frequency=0.2,
        ).until(lambda _driver: matching_elements() or False)
    except TimeoutException:
        return []


@step(u'I click the "{label}" button')
@persona_vars
def i_press_label_button(context, label):
    all_elements = find_button_with_label(context, label)
    case_insensitive_elements = [e for e in all_elements if
                                 str.upper(e.text) == str.upper(label)]
    case_sensitive_elements = [e for e in all_elements if e.text == label]
    elements = case_sensitive_elements or case_insensitive_elements
    if elements:
        element = elements[0]
    else:
        element = all_elements.first
    element.click()


@step(u'I click the last "{label}" button')
@persona_vars
def i_press_last_label_button(context, label):
    all_elements = find_button_with_label(context, label)
    case_insensitive_elements = [e for e in all_elements if
                                 str.upper(e.text) == str.upper(label)]
    case_sensitive_elements = [e for e in all_elements if e.text == label]
    elements = case_sensitive_elements or case_insensitive_elements
    if elements:
        element = elements[-1]
    else:
        element = all_elements.last
    element.click()


@step(u'I click the button with runtime text "{template}"')
@persona_vars
def i_click_button_with_runtime_text(context, template):
    i_press_label_button(context, render_runtime_template(context, template))


@step(u'I press the last element with xpath "{xpath}"')
@persona_vars
def i_press_last_xpath(context, xpath):
    button = context.browser.find_by_xpath(xpath)
    assert button, u"Element not found"
    button.last.click()


@step(u'I click the "{label}" checkbox')
@persona_vars
def i_press_label_checkbox(context, label):
    element = find_checkbox_with_label(context, label).first
    element.click()


@step(u'I click the "{label}" radio button')
@persona_vars
def i_press_label_radiobutton(context, label):
    element = find_radio_with_label(context, label).first
    element.click()


@step(u'I click the "{label}" card')
@persona_vars
def i_click_card_with_label(context, label):
    context.execute_steps(f"""
    When I press the last element with xpath "//div[contains(string(),'{label}')]"
    """)


@step(u'I should see the runtime string "{template}" within {timeout:d} seconds')
@persona_vars
def i_should_see_runtime_string(context, template, timeout):
    expected = render_runtime_template(context, template)
    found = context.browser.is_text_present(expected, wait_time=timeout)
    assert found, f'Did not see runtime string "{expected}"'


@step(u'I click the card with runtime text "{template}"')
@persona_vars
def i_click_card_with_runtime_text(context, template):
    i_click_card_with_label(context, render_runtime_template(context, template))


@step(u'I should see an element whose xpath "{xpath}" matches the runtime regex "{template}" within {timeout:d} seconds')
@persona_vars
def i_should_see_xpath_matching_runtime_regex(context, xpath, template, timeout):
    pattern = render_runtime_template(context, template, regex_escape_values=True)
    matching_elements = find_elements_by_xpath_matching_regex(
        context, xpath, pattern, timeout
    )
    assert matching_elements, (
        f'Did not see an element matching xpath "{xpath}" '
        f'and runtime regex "{pattern}"'
    )


@step(u'I press the last element whose xpath "{xpath}" matches the runtime regex "{template}"')
@persona_vars
def i_press_last_xpath_matching_runtime_regex(context, xpath, template):
    pattern = render_runtime_template(context, template, regex_escape_values=True)
    matching_elements = find_elements_by_xpath_matching_regex(context, xpath, pattern, 0)
    assert matching_elements, (
        f'Element not found for xpath "{xpath}" and runtime regex "{pattern}"'
    )
    matching_elements[-1].click()


@step("I enter OpenStack credentials")
@persona_vars
def i_login_to_exosphere(context):
    os_auth_url = os.environ.get('OS_AUTH_URL', 'https://js2.jetstream-cloud.org:5000/v3/')
    os_user_domain_name = os.environ.get('OS_USER_DOMAIN_NAME', 'access')
    os_username = os.environ.get('OS_USERNAME')
    os_password = os.environ.get('OS_PASSWORD')
    context.execute_steps(f"""
    Then I fill input labeled "Keystone auth URL" with "{os_auth_url}"
    Then I fill input labeled "User Domain (name or ID)" with "{os_user_domain_name}"
    Then I fill input labeled "User Name" with "{os_username}"
    Then I fill input labeled "Password" with "{os_password}"
    """)

@when('I add an OpenStack project "{project}"')
@persona_vars
def i_add_openstack_project(context, project):
    context.execute_steps(f"""
    When I click the "Add OpenStack Account" button
    Then I should see "Add an OpenStack Account" within 15 seconds
    When I enter OpenStack credentials
    And I click the "Log In" button
    Then I should see "{project}" within 15 seconds
    When I click the "{project}" checkbox
    And I click the "Choose" button
    Then I wait for 2 seconds
    """)


@step('a unique instance name starting with "{instance_name_begin}"')
@persona_vars
def unique_instance_name(context, instance_name_begin):
    context.unique_instance_name = f'{instance_name_begin}-{context.unique_tag}'


@step(u'I fill input labeled "{label}" with the unique instance name')
@persona_vars
def i_fill_input_labeled_with_unique_instance_name(context, label):
    element = find_input_by_label(context, label).first
    element.fill(context.unique_instance_name)


@step(u'I should see the unique instance name within {timeout:d} seconds')
@persona_vars
def see_unique_instance_name_within(context, timeout):
    context.execute_steps(f"""
    Then I should see an element with xpath "//div[contains(string(),'{context.unique_instance_name}')]" within {timeout} seconds
    """)


@step(u'I should not see the unique instance name within {timeout:d} seconds')
@persona_vars
def not_see_unique_instance_name_within(context, timeout):
    context.execute_steps(f"""
    Then I should not see an element with xpath "//div[contains(string(),'{context.unique_instance_name}')]" within {timeout} seconds
    """)


@step(u'I press on the unique instance name')
@persona_vars
def see_unique_instance_name_within(context):
    context.execute_steps(f"""
    When I press the last element with xpath "//div[contains(string(),'{context.unique_instance_name}')]"
    """)


@step(u'I press the "{option}" option in the "{label}" radio button group')
def press_radio_button_in_group(context, option, label):
    radio_group = context.browser.find_by_xpath(
        xpath=f"//label[@role='radiogroup' and contains(string(), '{label}')]")
    radio_button = radio_group.first.find_by_text(option)
    radio_button.click()

@then("I should see the CI commit short SHA in the UI")
def see_ci_commit_short_sha(context):
    expected = os.environ.get("CI_COMMIT_SHORT_SHA")
    assert expected, "CI_COMMIT_SHORT_SHA must be set in the environment"
    found = context.browser.is_text_present(expected, wait_time=5)
    assert found, f"Did not see CI_COMMIT_SHORT_SHA '{expected}' on the page"
