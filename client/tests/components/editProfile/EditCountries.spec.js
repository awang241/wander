import { shallowMount } from '@vue/test-utils'
import EditCountries from "../../../src/components/Profile/EditProfile/EditCountries";

let wrapper;

beforeEach(() => {
    wrapper = shallowMount(EditCountries, {
        propsData: {
            "profile" :{"chosenCountries": [],
            "newCountry": "",
            "chosenCountry": ""}
        },
        mocks: {},
        stubs: {},
        methods: {},
    });
    wrapper.vm.warningToast = jest.fn();
});

afterEach(() => {
    wrapper.destroy();
});

describe('EditCountries.vue', () => {
    test('Chosen countries start empty for a new profile', () => {
        wrapper.vm.chosenCountries = []
        expect(wrapper.vm.chosenCountries).toStrictEqual([]);
    });
    test('Add country with no name', () =>{
        wrapper.vm.newCountry = ""
        wrapper.vm.chosenCountries = []
        wrapper.vm.addCountry()
        expect(wrapper.vm.chosenCountries).toStrictEqual([]);
    });
    test('Add country with name', () =>{
        wrapper.vm.newCountry = "New Zealand"
        wrapper.vm.chosenCountries = []
        wrapper.vm.addCountry()
        expect(wrapper.vm.chosenCountries).toStrictEqual(["New Zealand"]);
    });
    test('Add existing country', () =>{
        wrapper.vm.newCountry = "New Zealand"
        wrapper.vm.chosenCountries = ["New Zealand"]
        wrapper.vm.addCountry()
        expect(wrapper.vm.chosenCountries).toStrictEqual(["New Zealand"]);
    });
    test('Delete country', () =>{
        wrapper.vm.chosenCountry = "New Zealand"
        wrapper.vm.chosenCountries = ["New Zealand"]
        wrapper.vm.deleteCountry(wrapper.vm.chosenCountry)
        expect(wrapper.vm.chosenCountries).toStrictEqual([]);
    });
});
